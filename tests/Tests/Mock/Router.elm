module Tests.Mock.Router exposing (..)

import Dict
import Html             exposing (Html)

import Router.Matcher as Matcher
import Router.Types        exposing (..)

-- import Response as R
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  }

bindForwardMock : RouterConfig route (WithRouter route state) msg -> Route route -> List (Html.Attribute (Msg route msg)) -> List (Html.Attribute (Msg route msg))
bindForwardMock config route attrs = attrs

buildUrlMock : RouterConfig route (WithRouter route state) msg -> Route route -> String
buildUrlMock (RouterConfig config) route =
  Matcher.buildUrl (.segment << config.routeConfig) (.parent << config.routeConfig) route

routerMock : RouterConfig route (WithRouter route state) msg -> Router route (WithRouter route state) msg
routerMock config =
  let
    (RouterConfig c) = config
  in {
    config = config
  , bindForward = bindForwardMock config
  , buildUrl = buildUrlMock config
  , forward = \_ -> Cmd.none
  , redirect = \_ -> Cmd.none
  , match = \_ -> Nothing
  }
