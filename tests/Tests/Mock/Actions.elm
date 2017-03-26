module Tests.Mock.Actions exposing (..)

import Tests.Mock.Routes exposing (Route)

import Router.Types exposing (WithRouter)
import Router.Types as Router
import Router.Actions exposing (..)
import Router exposing (initialState)

type Msg = NoOp | Succ | Append String

type alias State = WithRouter Route
  {
    str: String,
    sum: Int
  }

init : State
init = {
    router = initialState,
    str = "",
    sum = 0
  }

none : Action State Msg
none state = (state, Cmd.none)

succ : Action State Msg
succ state = noFx {state | sum = state.sum + 1}

append : String -> Action State Msg
append string state = noFx {state | str = state.str ++ string}

update : Msg -> Action State Msg
update msg = case msg of
  NoOp -> noFx
  Succ -> succ
  Append s -> append s
