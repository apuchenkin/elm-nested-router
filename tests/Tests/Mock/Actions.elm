module Tests.Mock.Actions exposing (..)

import Dict

import Tests.Mock.Routes exposing (Route)

import Router.Types    exposing (Action, WithRouter)
import Router.Types as Router
import Router.Helpers  exposing (noFx)

type Msg = NoOp | Succ | Append String

type alias State = WithRouter Route
  {
    str: String,
    sum: Int
  }

init : State
init = {
    router = {
      route = Nothing
    , arguments = Dict.empty
    },
    str = "",
    sum = 0
  }

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
