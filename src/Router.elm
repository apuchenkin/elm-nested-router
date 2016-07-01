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
    config' = RouterConfig <| { c | routeConfig = matcher.getConfig}
  in {
    config = config'
  , bindForward = bindForward config' matcher
  , buildUrl = buildUrl config' matcher
  , forward = forward config' matcher
  , redirect = redirect config' matcher
  , match = matchRoute matcher
  }

{-| Launches the router.
  Provide `init` function and router config as parameters
 -}
dispatch : (flags -> (WithRouter route state, Cmd (Action (WithRouter route state)))) -> RouterConfig route (WithRouter route state) -> Program flags -- flags
dispatch init config =
  let
    (RouterConfig c) = config
    matcher = Matcher.matcher config
    router = constructor config matcher

    getHandlers = createHandlers router matcher
    render' = render router <| List.map getHandlers << matcher.traverse
    urlUpdate route = runAction (transition router matcher getHandlers route)

    parser = Navigation.makeParser (matcher.match << getPath config)
    init' flags route =
      let
        (state, cmd) = init flags
        (state', cmd') = urlUpdate route state
      in (state', Cmd.batch [cmd, cmd'])
  in
    Navigation.programWithFlags parser
    {
      init = init'
    , update = runAction
    , urlUpdate = urlUpdate
    , view = render'
    , subscriptions = c.subscriptions
    }
