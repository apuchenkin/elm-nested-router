module Router exposing ( dispatch, dispatchWithFlags )
{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) ([Live demo](http://apuchenkin.github.io/elm-nested-router/example))
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details

@docs dispatch, dispatchWithFlags
-}

import Task
import Navigation       exposing (Location)

import Router.Types        exposing (..)
import Router.Functions    exposing (..)

{-| Launches the router.
  Provide `init` function and router config as parameters
 -}
dispatch : (WithRouter route state, Cmd (Msg route msg))
    -> RouterConfig route (WithRouter route state) msg
    -> Program Never (WithRouter route state) (Msg route msg)
dispatch (state, cmd) config =
  let
    (RouterConfig c) = config
    router = constructor config
    init location = (state, Cmd.batch [cmd, Task.perform identity <| Task.succeed <| Transition location])

    args = {
      init = init
    , update = update router
    , view = render router
    , subscriptions = c.subscriptions
    }

  in Navigation.program Transition args

{-| Launches the router.
  Provide `init` function and router config as parameters
 -}
dispatchWithFlags : (flags -> (WithRouter route state, Cmd (Msg route msg)))
    -> RouterConfig route (WithRouter route state) msg
    -> Program flags (WithRouter route state) (Msg route msg)
dispatchWithFlags getInitialState config =
  let
    (RouterConfig c) = config
    router = constructor config
    init_mod flags location =
      let (state, cmd) = getInitialState flags
      in (state, Cmd.batch [cmd, Task.perform identity <| Task.succeed <| Transition location])

    args = {
      init = init_mod
    , update = update router
    , view = render router
    , subscriptions = c.subscriptions
    }

  in Navigation.programWithFlags Transition args
