module Router exposing ( dispatch, initialState )
{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) ([Live demo](http://apuchenkin.github.io/elm-nested-router/example))
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details

@docs dispatch, initialState
-}

import Dict
import Navigation       exposing (Location)

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)
import Router.Functions    exposing (..)
import Router.Navigation   exposing (..)

{-| Initial state for router. Fed this into your application state -}
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  }

{-| Router constructor -}
constructor : RouterConfig route state -> Matcher route state -> Router route state
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
dispatch : (WithRouter route state, Cmd (Action (WithRouter route state)))
    -> RouterConfig route (WithRouter route state)
    -> Program Never (WithRouter route state) (Action (WithRouter route state))
dispatch init config =
  let
    (RouterConfig c) = config
    matcher = Matcher.matcher config
    router = constructor config matcher

    getHandlers = createHandlers router matcher
    render_view = render router <| List.map getHandlers << matcher.traverse

    urlUpdate route =  transition router matcher getHandlers route

    updateAction : Location -> Action (WithRouter route state)
    updateAction = urlUpdate << matcher.match << getPath config

    init_mod location =
      let
        (state, cmd) = init
        (state_new, cmd_new) = runAction (updateAction location) state
      in (state_new, Cmd.batch [cmd, cmd_new])

    args = {
      init = init_mod
    , update = runAction
    , view = render_view
    , subscriptions = c.subscriptions
    }

  in Navigation.program updateAction args

{-| Launches the router.
  Provide `init` function and router config as parameters
 -}
dispatchWithFlags : (flags -> (WithRouter route state, Cmd (Action (WithRouter route state))))
    -> RouterConfig route (WithRouter route state)
    -> Program flags (WithRouter route state) (Action (WithRouter route state))
dispatchWithFlags init config =
  let
    (RouterConfig c) = config
    matcher = Matcher.matcher config
    router = constructor config matcher

    getHandlers = createHandlers router matcher
    render_view = render router <| List.map getHandlers << matcher.traverse

    urlUpdate route =  transition router matcher getHandlers route

    updateAction : Location -> Action (WithRouter route state)
    updateAction = urlUpdate << matcher.match << getPath config

    init_mod flags location =
      let
        (state, cmd) = init flags
        (state_new, cmd_new) = runAction (updateAction location) state
      in (state_new, Cmd.batch [cmd, cmd_new])

    args = {
      init = init_mod
    , update = runAction
    , view = render_view
    , subscriptions = c.subscriptions
    }

  in Navigation.programWithFlags updateAction args
