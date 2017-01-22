module Router exposing ( initialState, dispatch, dispatchWithFlags )
{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) ([Live demo](http://apuchenkin.github.io/elm-nested-router/example))
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details

@docs initialState, dispatch, dispatchWithFlags
-}

import Dict
import Task
import Navigation       exposing (Location)

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)
import Router.Functions    exposing (..)
import Router.Navigation   exposing (..)
-- import Router.Helpers      exposing (chainAction)

{-| Initial state for router. Fed this into your application state -}
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  }

{-| Router constructor -}
constructor : RouterConfig route state msg -> Matcher route state msg -> Router route state msg
constructor config matcher =
  let
    (RouterConfig c) = config
    config_new = RouterConfig <| { c | routeConfig = matcher.getConfig}
  in {
    config = config_new
  , bindForward = bindForward config_new matcher
  , buildUrl = buildUrl config_new matcher
  , forward = forward config_new matcher
  , redirect = redirect config_new matcher
  , match = matchRoute matcher
  }

{-| Launches the router.
  Provide `init` function and router config as parameters
 -}
dispatch : (WithRouter route state, Cmd (Msg route msg))
    -> RouterConfig route (WithRouter route state) msg
    -> Program Never (WithRouter route state) (Msg route msg)
dispatch (state, cmd) config =
  let
    (RouterConfig c) = config
    matcher = Matcher.matcher config
    router = constructor config matcher
    getHandlers = createHandlers router matcher
    init location = (state, Cmd.batch [cmd, Task.perform identity <| Task.succeed <| Transition location])

    args = {
      init = init
    , update = update router matcher getHandlers
    , view = render router <| List.map getHandlers << matcher.traverse
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
    matcher = Matcher.matcher config
    router = constructor config matcher
    getHandlers = createHandlers router matcher
    init_mod flags location =
      let (state, cmd) = getInitialState flags
      in (state, Cmd.batch [cmd, Task.perform identity <| Task.succeed <| Transition location])

    args = {
      init = init_mod
    , update = update router matcher getHandlers
    , view = render router <| List.map getHandlers << matcher.traverse
    , subscriptions = c.subscriptions
    }

  in Navigation.programWithFlags Transition args
