module App.Layout where

import Dict exposing (Dict)
import Html exposing (Html)
import App.Actions exposing (State)
import App.Routes as Route exposing (Route)
import Router.Types exposing (Router (..))

footer : Router Route State -> Html
footer router =
  let
    (Router r) = router
    about    = Html.a (r.bindForward (Route.Static "about",    Dict.empty) []) [Html.text "About"]
    contacts = Html.a (r.bindForward (Route.Static "contacts", Dict.empty) []) [Html.text "Contacts"]
    sep = Html.text " | "
  in Html.footer [] [
    about, sep, contacts
  ]

layout : Router Route State -> State -> Dict String Html -> Html
layout router _ parsed =
  let
    defaultHeader = Html.header [] [Html.text "Default header"]
    defaultFooter = footer router
    defaultBody = Html.div [] [Html.text "empty"]
  in Html.div [] [
    Maybe.withDefault defaultHeader <| Dict.get "header" parsed
  , Maybe.withDefault defaultBody   <| Dict.get "body" parsed
  , Maybe.withDefault defaultFooter <| Dict.get "footer" parsed
  ]
