module Test.Mock.Data where

import Html exposing (Html)
import Dict exposing (Dict)
import MultiwayTree exposing (Tree (..), Forest)

import Router.Helpers  exposing (noFx)
import Router.Types    exposing (..)

import Test.Mock.Router exposing (..)

type Route = Home | Page | Subpage | NotFound

routeTree : Forest Route
routeTree = [
    Tree NotFound [],
    Tree Home [
      Tree Page [
        Tree Subpage []
      ]
    ]
  ]

type alias State = WithRouter Route
  {
    str: String,
    sum: Int
  }

config : Route -> RouteConfig Route State
config route = case route of
    Home        -> {
      segment = "/"
    , constraints = Dict.empty
    , handler = always handlerA
    }
    NotFound    -> {
      segment = "/404"
    , constraints = Dict.empty
    , handler = always handlerA
    }
    Page        -> {
      segment = ":category[/:subcategory]"
    , constraints = Dict.empty
    , handler = always handlerB
    }
    Subpage     -> {
      segment = "/item/:item"
    , constraints = Dict.empty
    , handler = always handlerC
    }

routeMap : Route -> (RawURL, Dict String Constraint)
routeMap route = (.segment <| config route, .constraints <| config route)

init : State
init = {
    router = initialState,
    str = "",
    sum = 0
  }

layout : Dict String Html -> Html
layout parsed =
  let fallback = Html.text "error"
  in Maybe.withDefault (Maybe.withDefault (Maybe.withDefault fallback
    <| Dict.get "handlerA" parsed)
    <| Dict.get "handlerB" parsed)
    <| Dict.get "handlerC" parsed

routerConfig : RouterConfig Route State
routerConfig = {
    init      = init,
    useCache  = True,
    fallback  = (NotFound, Dict.empty),
    layout    = layout,
    routes    = routeTree,
    config    = config,
    inits  = [],
    inputs = []
  }
------------ actions ----------------------
noAction : Action State
noAction state = Response <| noFx state

succ : Action State
succ state = Response <| noFx {state | sum = state.sum + 1}

append : String -> Action State
append string state = Response <| noFx {state | str = state.str ++ string}

------------ handlers ----------------------

-- only text html can be tested due to bug (toString(null))

handlerA : Handler State
handlerA = {
    view = \address state parsed -> Dict.fromList [("handlerA", Html.text "handlerA")],
    actions = [
      noAction
    ]
  }

handlerB : Handler State
handlerB = {
    view = \address state parsed -> Dict.fromList [("handlerB", Html.text <| toString state.sum)],
    actions = [
      succ
    ]
  }

handlerC : Handler State
handlerC = {
    view = \address state parsed -> Dict.fromList [("handlerC", Html.text state.str)],
    actions = [
      succ,
      append "foo"
    ]
  }

router : Router Route State
router = routerMock routerConfig
