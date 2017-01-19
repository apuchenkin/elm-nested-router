module Tests.Mock.Data exposing (..)

import Html exposing (Html)
import Dict exposing (Dict)

import Router.Helpers  exposing (noFx)
import Router.Types    exposing (..)
import Router.Types as Router
import Router.Matcher as Matcher exposing (Matcher)
import Router.Functions exposing (createHandlers)
import Tests.Mock.Router exposing (..)

type Route = Home | Page | Subpage | NotFound

routes : List Route
routes = [NotFound, Home, Page, Subpage]

type alias State = WithRouter Route
  {
    str: String,
    sum: Int
  }

config : Route -> RouteConfig Route State Msg
config route = case route of
    Home -> {
      segment = "/"
    , bypass = False
    , parent = Nothing
    , constraints = Dict.empty
    , handler = always handlerA
    }
    NotFound -> {
      segment = "/404"
    , bypass = False
    , parent = Nothing
    , constraints = Dict.empty
    , handler = always handlerA
    }
    Page -> {
      segment = ":category[/:subcategory]"
    , bypass = False
    , parent = Just Home
    , constraints = Dict.fromList [("category", Enum ["A","B","C"])]
    , handler = always handlerB
    }
    Subpage -> {
      segment = "/item/:item"
    , bypass = False
    , parent = Just Page
    , constraints = Dict.fromList [("item", Int)]
    , handler = always handlerC
    }

init : State
init = {
    router = initialState,
    str = "",
    sum = 0
  }

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
  -- , transition = \_ _ _ -> doNothing
  , routes = routes
  , routeConfig = config
  , subscriptions = always Sub.none
  }
------------ actions ----------------------

type Msg = NoOp | Succ | Append String

noAction : Action State (Router.Msg Route Msg)
noAction state = noFx state

succ : Action State (Router.Msg Route Msg)
succ state = noFx {state | sum = state.sum + 1}

append : String -> Action State (Router.Msg Route Msg)
append string state = noFx {state | str = state.str ++ string}

update : Msg -> Action State (Router.Msg Route Msg)
update msg = case msg of
  NoOp -> noAction
  Succ -> succ
  Append s -> append s
------------ handlers ----------------------

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

matcher : Matcher Route State Msg
matcher = Matcher.matcher routerConfig

getHandlers : Route -> Handler Route State Msg
getHandlers = createHandlers router matcher
