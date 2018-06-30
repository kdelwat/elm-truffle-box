port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Eth
import Eth.Decode
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Types exposing (..)
import Eth.Utils
import Eth.Units
import Evm.Decode as Evm
import Evm.Encode as Evm
import Json.Decode as Decode exposing (Value)
import BigInt exposing (BigInt)
import Task
import Http

ethNodeAddress : String
ethNodeAddress = "http://localhost:9545"

contractAddress : Address
contractAddress = Eth.Utils.unsafeToAddress "0x345ca3e014aaf5dca488057592ee47305d9b3e10"

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Model = {
    txSentry : TxSentry Msg,
    ethNode : HttpProvider,
    inputValue : String,
    number : String,
    account : Maybe Address,
    messages : List String
}


modelInitialValue : Model
modelInitialValue =
    { ethNode = ethNodeAddress
    , inputValue = ""
    , number = ""
    , account = Nothing
    , txSentry = TxSentry.init (txOut, txIn) TxSentryMsg ethNodeAddress
    , messages = []
    }


type Msg
    = Change String
    | GetValue
    | SetValue String
    | ReceiveNumber (Result Http.Error BigInt)
    | ReceiveSetResult (Result Http.Error TxHash)
    | TxSentryMsg TxSentry.Msg
    | WatchTxReceipt TxReceipt
    | WatchTxBroadcast Tx
    | SetAccount (Maybe Address)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TxSentryMsg subMsg ->
            let
                (subModel, subCmd) =
                    TxSentry.update subMsg model.txSentry
            in
                ({model | txSentry = subModel}, subCmd)
        Change newInputValue ->
            ({model | inputValue = newInputValue}, Cmd.none)
        GetValue ->
            (model, getValue model.ethNode)
        WatchTxReceipt txReceipt ->
            ({ model | messages = receiptToMessage txReceipt :: model.messages }, Cmd.none)
        WatchTxBroadcast tx ->
            ({ model | messages = txToMessage tx :: model.messages }, Cmd.none)
        SetValue value ->
            let
                newValue = Maybe.withDefault (BigInt.fromInt 0) (BigInt.fromString value)
                            
                params = set contractAddress model.account newValue
                
                (newSentry, sentryCmd) =
                    TxSentry.customSend
                        model.txSentry
                        { onSign = Nothing
                        , onBroadcast = Just WatchTxBroadcast
                        , onMined = Just (WatchTxReceipt, Nothing) }
                        (Eth.toSend params)
            in
                ({ model | txSentry = newSentry}, sentryCmd)
        ReceiveNumber (Ok newNumber) -> 
            ({model | number = (BigInt.toString newNumber)}, Cmd.none)
        ReceiveNumber (Err error) -> 
            ({model | number = (toString error)}, Cmd.none)
        ReceiveSetResult (Ok _) ->
            (model, Cmd.none)
        ReceiveSetResult (Err error) ->
            ({model | number = (toString error)}, Cmd.none)
        SetAccount account ->
            ({model | account = account}, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "SimpleStorage" ]
        , p [] [ text ("Current value: " ++ model.number)]
        , div
            []
            [ button [ onClick GetValue ] [ text "Get" ]
            , button [ onClick (SetValue model.inputValue) ] [ text "Set"]
            , input [ placeholder "New value", onInput Change ] []
            ]
        , h2 [] [ text "Activity log" ]
        , div [] (List.map (\m -> p [] [text m]) model.messages)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ accountListener accountListenerToMsg
        , TxSentry.listen model.txSentry
        ]


init : (Model, Cmd Msg)
init = 
    (modelInitialValue, Cmd.none)

getValue : String -> Cmd Msg
getValue node =
    let
        call = get contractAddress
    in
        Eth.call node call
            |> Task.attempt ReceiveNumber

get : Address -> Call BigInt
get contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "get()" []
    , nonce = Nothing
    , decoder = Evm.toElmDecoder Evm.uint}

set : Address -> Maybe Address -> BigInt -> Call Bool
set contractAddress account value =
    { to = Just contractAddress
    , from = account
    , gas = Nothing
    , gasPrice = Just <| Eth.Units.gwei 4
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "set(uint256)" [Evm.UintE value]
    , nonce = Nothing
    , decoder = Evm.toElmDecoder Evm.bool
    }

receiptToMessage : TxReceipt -> String
receiptToMessage receipt =
    "Mined: " ++ toString receipt.hash

txToMessage : Tx -> String
txToMessage tx =
    "Broadcast: " ++ toString tx.hash

accountListenerToMsg : Value -> Msg
accountListenerToMsg val =
    Decode.decodeValue Eth.Decode.address val
        |> Result.toMaybe
        |> SetAccount

port accountListener : (Value -> msg) -> Sub msg

port txOut : Value -> Cmd msg

port txIn : (Value -> msg) -> Sub msg