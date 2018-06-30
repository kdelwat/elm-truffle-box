import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Eth
import Eth.Types exposing (..)
import Eth.Utils
import Eth.Units
import Evm.Decode as Evm
import Evm.Encode as Evm
import BigInt exposing (BigInt)
import Task
import Http

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model = {
    ethNode : HttpProvider,
    inputValue : String,
    number : String
}

modelInitialValue : Model
modelInitialValue = { ethNode = "http://localhost:9545", inputValue = "", number = "" }


type Msg
    = Change String
    | GetValue
    | SetValue String
    | ReceiveNumber (Result Http.Error BigInt)
    | ReceiveSetResult (Result Http.Error TxHash)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Change newInputValue ->
            ({model | inputValue = newInputValue}, Cmd.none)
        GetValue -> 
            (model, getValue model.ethNode)
        SetValue value ->
            (model, setValue model.ethNode (Maybe.withDefault (BigInt.fromInt 0) (BigInt.fromString value)))
        ReceiveNumber (Ok newNumber) -> 
            ({model | number = (BigInt.toString newNumber)}, Cmd.none)
        ReceiveNumber (Err error) -> 
            ({model | number = (toString error)}, Cmd.none)
        ReceiveSetResult (Ok _) ->
            (model, Cmd.none)
        ReceiveSetResult (Err error) ->
            ({model | number = (toString error)}, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "SimpleStorage"]
        , p [] [ text ("Current value: " ++ model.number)]
        , div
            []
            [ button [ onClick GetValue ] [ text "Get" ]
            , button [ onClick (SetValue model.inputValue) ] [ text "Set"]
            , input [ placeholder "New value", onInput Change ] []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    (modelInitialValue, Cmd.none)

getValue : String -> Cmd Msg
getValue node =
    let
        call = get (Eth.Utils.unsafeToAddress "0x345ca3e014aaf5dca488057592ee47305d9b3e10")
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

setValue : String -> BigInt -> Cmd Msg
setValue node value =
    let
        call = set (Eth.Utils.unsafeToAddress "0x345ca3e014aaf5dca488057592ee47305d9b3e10") value
    in
        Eth.sendTx node (Eth.toSend call)
            |> Task.attempt ReceiveSetResult

set : Address -> BigInt -> Call Bool
set contractAddress value =
    { to = Just contractAddress
    , from = Just (Eth.Utils.unsafeToAddress "0x627306090abab3a6e1400e9345bc60c78a8bef57")
    , gas = Nothing
    , gasPrice = Just <| Eth.Units.gwei 4
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "set(uint256)" [Evm.UintE value]
    , nonce = Nothing
    , decoder = Evm.toElmDecoder Evm.bool
    }