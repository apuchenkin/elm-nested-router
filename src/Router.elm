module Router exposing ( dispatch, dispatchWithFlags, initialState )
{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) ([Live demo](http://apuchenkin.github.io/elm-nested-router/example))
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details

@docs dispatch, dispatchWithFlags, initialState
-}

import Dict
import Task
import Navigation exposing (Location)

import Router.Types exposing (RouterState, Router, RouterConfig (..), WithRouter)
import Router.Functions exposing (..)
import Router.Navigation exposing (..)
import Router.Actions exposing (..)

{-| Initial state for router. Fed this into your application state -}
initialState : RouterState route
initialState = {
    route = Nothing
  , arguments = Dict.empty
  }

{-| Router constructor -}
constructor : RouterConfig route state msg -> Router route state msg
constructor config = {
    config = config
  , buildUrl = buildUrl config
  , forward = forward config
  , redirect = redirect config
  }

bootstrap : (state, Cmd msg) -> Location -> (state, Cmd (Msg route msg))
bootstrap (state, cmd) location = (state, Cmd.batch [Cmd.map AppMsg cmd, Task.perform identity <| Task.succeed <| Transition location])

{-| Launches the router.
  Provide `init` function and router config as parameters
 -}
dispatch : (WithRouter route state, Cmd msg)
    -> RouterConfig route (WithRouter route state) msg
    -> Program Never (WithRouter route state) (Msg route msg)
dispatch initial config =
  let
    (RouterConfig c) = config
    router = constructor config
  in Navigation.program Transition {
    init = bootstrap initial
  , update = update router
  , view = render router
  , subscriptions = Sub.map AppMsg << c.subscriptions
  }

{-| Launches the router.
  Provide `init` function and router config as parameters
 -}
dispatchWithFlags : (flags -> (WithRouter route state, Cmd msg))
    -> RouterConfig route (WithRouter route state) msg
    -> Program flags (WithRouter route state) (Msg route msg)
dispatchWithFlags initial config =
  let
    (RouterConfig c) = config
    router = constructor config
  in Navigation.programWithFlags Transition {
    init = bootstrap << initial
  , update = update router
  , view = render router
  , subscriptions = Sub.map AppMsg << c.subscriptions
  }
