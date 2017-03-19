module Tests.Mock.Router exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)

import Matcher.Matcher as Matcher
import Router.Types as Router
import Tests.Mock.Actions exposing (..)
import Tests.Mock.Routes exposing (..)
import Tests.Mock.Handlers as Handlers

-- import Response as R

bindForwardMock :
  Router.RouterConfig route (Router.WithRouter route state) msg ->
  Matcher.Route route -> List (Html.Attribute (Router.Msg route msg)) ->
  List (Html.Attribute (Router.Msg route msg))
bindForwardMock config route attrs = attrs

buildUrlMock : Router.RouterConfig route (Router.WithRouter route state) msg -> Matcher.Route route -> String
buildUrlMock (Router.RouterConfig config) route =
  Matcher.buildURL (.route << config.routeConfig) route

routerMock : Router.RouterConfig route (Router.WithRouter route state) msg -> Router.Router route (Router.WithRouter route state) msg
routerMock config =
  let
    (Router.RouterConfig c) = config
  in {
    config = config
  , bindForward = bindForwardMock config
  , buildUrl = buildUrlMock config
  , forward = \_ -> Cmd.none
  , redirect = \_ -> Cmd.none
  , match = \_ -> Nothing
  }

-- layout : Router.Render Route State (Router.Msg Route Msg)
layout _ _ views =
  let fallback = Html.text "error"
  in Maybe.withDefault (Maybe.withDefault (Maybe.withDefault (Maybe.withDefault fallback
    <| Dict.get "home" views)
    <| Dict.get "category" views)
    <| Dict.get "post" views)
    <| Dict.get "article" views

routerConfig : Router.RouterConfig Route State Msg
routerConfig = Router.RouterConfig {
    html5 = True
  , removeTrailingSlash = True
  , layout = layout
  , update = update
  , onTransition = \_ _ _ -> []
  , routes = routes
  , routeConfig = Handlers.routeConfig
  , subscriptions = always Sub.none
  }

router : Router.Router Route State Msg
router = routerMock routerConfig
