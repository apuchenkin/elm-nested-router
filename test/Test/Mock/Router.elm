module Test.Mock.Router where

import Dict
import Html             exposing (Html)

import Router.Matcher as Matcher
import Router.Helpers      exposing (..)
import Router.Types        exposing (..)
import Router.Mailbox      exposing (address)

-- import Response as R
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  }

bindForwardMock : RouterConfig route (WithRouter route state) -> Route route -> List Html.Attribute -> List Html.Attribute
bindForwardMock config route attrs = attrs

buildUrlMock : RouterConfig route (WithRouter route state) -> Route route -> String
buildUrlMock (RouterConfig config) route =
  Matcher.buildUrl (.segment << config.routeConfig) (.parent << config.routeConfig) route

routerMock : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
routerMock config =
  let
    (RouterConfig c) = config
  in {
    config        = config
  , address       = address
  , bindForward   = bindForwardMock   config
  , buildUrl      = buildUrlMock      config
  , forward       = \route state -> Response <| noFx state
  , redirect      = \route state -> Response <| noFx state
  , match         = \_ -> Nothing
  }
