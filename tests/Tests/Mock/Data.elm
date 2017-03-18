module Tests.Mock.Data exposing (..)

import Html exposing (Html)
import Dict exposing (Dict)

import Router.Helpers  exposing (noFx)
import Router.Types    exposing (..)
import Router.Types as Router
-- import Router.Matcher as Matcher exposing (Matcher)
import Router.Functions exposing (createHandlers)
import Tests.Mock.Router exposing (..)

layout : Router Route State Msg -> State -> Dict String (Html (Router.Msg Route Msg)) -> (Html (Router.Msg Route Msg))
layout _ _ parsed =
  let fallback = Html.text "error"
  in Maybe.withDefault (Maybe.withDefault (Maybe.withDefault fallback
    <| Dict.get "handlerA" parsed)
    <| Dict.get "handlerB" parsed)
    <| Dict.get "handlerC" parsed

routerConfig : RouterConfig Route State Msg
routerConfig = RouterConfig {
    html5 = True
  , removeTrailingSlash = True
  , layout = layout
  , update = update
  , onTransition = \_ _ _ -> []
  , routes = routes
  , routeConfig = config
  , subscriptions = always Sub.none
  }

-- only text html can be tested due to bug (toString(null))

handlerA : Handler Route State Msg
handlerA = {
    view = \state _ -> Dict.fromList [("handlerA", Html.text "handlerA")],
    actions = [
      NoOp
    ]
  }

handlerB : Handler Route State Msg
handlerB = {
    view = \state _ -> Dict.fromList [("handlerB", Html.text <| toString state.sum)],
    actions = [
      Succ
    ]
  }

handlerC : Handler Route State Msg
handlerC = {
    view = \state _ -> Dict.fromList [("handlerC", Html.text state.str)],
    actions = [
      Succ,
      Append "foo"
    ]
  }

router : Router Route State Msg
router = routerMock routerConfig

-- matcher : Matcher Route State Msg
-- matcher = Matcher.matcher routerConfig

getHandlers : Route -> Handler Route State Msg
getHandlers = createHandlers router matcher
