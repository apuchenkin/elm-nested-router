module App.Layout exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import App.Actions exposing (State, Msg)
import App.Routes as Route exposing (Route)
import Router.Types exposing (Router)
import Router.Types as Router
import Matcher.Matcher exposing (route)

footer : Router Route State Msg -> Html (Router.Msg Route Msg)
footer router =
  let
    about    = Html.a (router.bindForward (route (Route.Static "about") Dict.empty) []) [Html.text "About"]
    contacts = Html.a (router.bindForward (route (Route.Static "contacts") Dict.empty) []) [Html.text "Contacts"]
    sep = Html.text " | "
  in Html.footer [] [
    about, sep, contacts
  ]

layout : Router Route State Msg -> State -> Dict String (Html (Router.Msg Route Msg)) -> Html (Router.Msg Route Msg)
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
