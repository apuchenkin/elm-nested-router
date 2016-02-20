module Test.Mock.Data where

import Html
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

routerConfig : RouterConfig Route State
routerConfig = {
    init      = init,
    useCache  = True,
    fallback  = (NotFound, Dict.empty),
    fallbackHtml  = Html.text "error",
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
    view = \address state parsed -> Just <| Maybe.withDefault (Html.text "handlerA") parsed,
    actions = [
      noAction
    ]
  }

handlerB : Handler State
handlerB = {
    view = \address state parsed -> Just <| Maybe.withDefault (Html.text <| toString state.sum) parsed,
    actions = [
      succ
    ]
  }

handlerC : Handler State
handlerC = {
    view = \address state parsed -> Just <| Maybe.withDefault (Html.text state.str) parsed,
    actions = [
      succ,
      append "foo"
    ]
  }

router : Router Route State
router = routerMock routerConfig
