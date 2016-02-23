module App.Layout where

import Dict exposing (Dict)
import Html exposing (Html)

layout : Dict String Html -> Html
layout parsed =
  let
    defaultHeader = Html.header [] [Html.text "Default header"]
    defaultFooter = Html.footer [] [Html.text "Default footer"]
    defaultBody = Html.div [] [Html.text "empty"]
  in Html.div [] [
    Maybe.withDefault defaultHeader <| Dict.get "header" parsed
  , Maybe.withDefault defaultBody   <| Dict.get "body" parsed
  , Maybe.withDefault defaultFooter <| Dict.get "footer" parsed
  ]
