module App.Layout exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import App.Actions exposing (State)
import App.Routes as Route exposing (Route)
import Router.Types exposing (Router, Action)

footer : Router Route State -> Html (Action State)
footer router =
  let
    about    = Html.a (router.bindForward (Route.Static "about",    Dict.empty) []) [Html.text "About"]
    contacts = Html.a (router.bindForward (Route.Static "contacts", Dict.empty) []) [Html.text "Contacts"]
    sep = Html.text " | "
  in Html.footer [] [
    about, sep, contacts
  ]

layout : Router Route State -> State -> Dict String (Html (Action State)) -> Html (Action State)
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
