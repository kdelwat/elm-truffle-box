module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Update exposing (Model, Msg)


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "SimpleStorage" ]
        , p [] [ text ("Current value: " ++ model.number) ]
        , div
            []
            [ button [ onClick Update.GetValue ] [ text "Get" ]
            , button [ onClick (Update.SetValue model.inputValue) ] [ text "Set" ]
            , input [ placeholder "New value", onInput Update.Change ] []
            ]
        , h2 [] [ text "Activity log" ]
        , div [] (List.map (\m -> p [] [ text m ]) model.messages)
        ]
