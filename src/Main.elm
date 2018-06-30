port module Main exposing (..)

import Eth.Decode
import Eth.Sentry.Tx as TxSentry
import Html
import Json.Decode as Decode exposing (Value)
import Update exposing (Model, Msg, update)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


modelInitialValue : Model
modelInitialValue =
    { ethNode = Update.ethNodeAddress
    , inputValue = ""
    , number = ""
    , account = Nothing
    , txSentry = TxSentry.init ( txOut, txIn ) Update.TxSentryMsg Update.ethNodeAddress
    , messages = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ accountListener accountListenerToMsg
        , TxSentry.listen model.txSentry
        ]


init : ( Model, Cmd Msg )
init =
    ( modelInitialValue, Cmd.none )


accountListenerToMsg : Value -> Msg
accountListenerToMsg val =
    Decode.decodeValue Eth.Decode.address val
        |> Result.toMaybe
        |> Update.SetAccount


port accountListener : (Value -> msg) -> Sub msg


port txOut : Value -> Cmd msg


port txIn : (Value -> msg) -> Sub msg
