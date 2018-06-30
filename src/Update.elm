module Update exposing (..)

import BigInt exposing (BigInt)
import Contract exposing (get, set)
import Eth
import Eth.Sentry.Tx as TxSentry exposing (..)
import Eth.Types exposing (Address, HttpProvider, Tx, TxHash, TxReceipt)
import Eth.Utils
import Http
import Task


ethNodeAddress : String
ethNodeAddress =
    "http://localhost:9545"


contractAddress : Address
contractAddress =
    Eth.Utils.unsafeToAddress "0x345ca3e014aaf5dca488057592ee47305d9b3e10"


type alias Model =
    { txSentry : TxSentry Msg
    , ethNode : HttpProvider
    , inputValue : String
    , number : String
    , account : Maybe Address
    , messages : List String
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TxSentryMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    TxSentry.update subMsg model.txSentry
            in
            ( { model | txSentry = subModel }, subCmd )

        Change newInputValue ->
            ( { model | inputValue = newInputValue }, Cmd.none )

        GetValue ->
            ( model, getValue model.ethNode )

        WatchTxReceipt txReceipt ->
            ( { model | messages = receiptToMessage txReceipt :: model.messages }, Cmd.none )

        WatchTxBroadcast tx ->
            ( { model | messages = txToMessage tx :: model.messages }, Cmd.none )

        SetValue value ->
            let
                newValue =
                    Maybe.withDefault (BigInt.fromInt 0) (BigInt.fromString value)

                params =
                    set contractAddress model.account newValue

                ( newSentry, sentryCmd ) =
                    TxSentry.customSend
                        model.txSentry
                        { onSign = Nothing
                        , onBroadcast = Just WatchTxBroadcast
                        , onMined = Just ( WatchTxReceipt, Nothing )
                        }
                        (Eth.toSend params)
            in
            ( { model | txSentry = newSentry }, sentryCmd )

        ReceiveNumber (Ok newNumber) ->
            ( { model | number = BigInt.toString newNumber }, Cmd.none )

        ReceiveNumber (Err error) ->
            ( { model | number = toString error }, Cmd.none )

        ReceiveSetResult (Ok _) ->
            ( model, Cmd.none )

        ReceiveSetResult (Err error) ->
            ( { model | number = toString error }, Cmd.none )

        SetAccount account ->
            ( { model | account = account }, Cmd.none )


getValue : String -> Cmd Msg
getValue node =
    let
        call =
            get contractAddress
    in
    Eth.call node call
        |> Task.attempt ReceiveNumber


receiptToMessage : TxReceipt -> String
receiptToMessage receipt =
    "Mined: " ++ toString receipt.hash


txToMessage : Tx -> String
txToMessage tx =
    "Broadcast: " ++ toString tx.hash
