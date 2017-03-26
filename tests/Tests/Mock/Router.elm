module Tests.Mock.Router exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)

import Router.Types as Router
import Tests.Mock.Actions exposing (..)
import Tests.Mock.Routes exposing (..)
import Tests.Mock.RouteConfig exposing (routeConfig)
import Router.Navigation exposing (..)

layout : a -> b -> Dict String (Html msg) -> Html msg
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
  , routeConfig = routeConfig
  , subscriptions = always Sub.none
  }

router : Router.Router Route State Msg
router = {
    config = routerConfig
  , bindForward = bindForward routerConfig
  , buildUrl = buildUrl routerConfig
  , forward = forward routerConfig
  , redirect = redirect routerConfig
  }
