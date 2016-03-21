module Router (runRouter, initialState) where
{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example)
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details
([Live demo](http://apuchenkin.github.io/elm-nested-router/example))

@docs runRouter, initialState
-}

import Dict
import String
import History
import Html             exposing (Html)
import Effects          exposing (Effects)
import Signal.Extra     exposing (fairMerge, foldp')
import Task             exposing (Task)
import Html.Events      exposing (onWithOptions)
import Html.Attributes  as Attr
import Json.Decode      as Json

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Helpers      exposing (..)
import Router.Types        exposing (..)
import Router.Functions    exposing (..)
import Router.Mailbox      exposing (..)

hash : Char
hash = '#'

{-| Initial state for router. Fed this into your application state -}
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  }

{-| binds forward action to existing HTML attributes. Exposed by `Router` -}
bindForward : RouterConfig route (WithRouter route state) -> Matcher route (WithRouter route state) -> Route route -> List Html.Attribute -> List Html.Attribute
bindForward config matcher route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    action _ = Signal.message address <| forward config matcher route
  in
    Attr.href (buildUrl config matcher route)
    :: onWithOptions "click" options Json.value action
    :: attrs

{-| Decomposes Route to string. Exposed by `Router` -}
buildUrl : RouterConfig route (WithRouter route state) -> Matcher route (WithRouter route state) -> Route route -> String
buildUrl routerConfig matcher (route, params) =
  let
    (RouterConfig config) = routerConfig
    url = matcher.buildUrl (route, params)
    url' = if config.removeTrailingSlash then Matcher.removeTrailingSlash url else url
    url'' = if config.html5 then url' else String.cons hash url'
  in url''

{-| Preforms a transition to provided `Route`. Exposed by `Router` -}
forward : RouterConfig route (WithRouter route state) -> Matcher route (WithRouter route state) -> Route route -> Action (WithRouter route state)
forward routerConfig matcher route state =
  let
    (RouterConfig config) = routerConfig
    url   = buildUrl routerConfig matcher route
    task  = History.setPath url |> Task.map (always doNothing)
  in Response (state, Effects.task task)

{-| Redirects to provided `Route`. Exposed by `Router` -}
redirect : RouterConfig route (WithRouter route state) -> Matcher route (WithRouter route state) -> Route route -> Action (WithRouter route state)
redirect routerConfig matcher route state =
  let
    (RouterConfig config) = routerConfig
    url   = buildUrl routerConfig matcher route
    task  = History.replacePath url |> Task.map (always doNothing)
  in Response (state, Effects.task task)

{-| @Private
  Preforms attempt to match provided url to a route by a given routes configuration
  -}
matchRoute : Matcher route state -> String -> Maybe (Route route)
matchRoute matcher url = matcher.match url

{-| Router constructor -}
constructor : RouterConfig route (WithRouter route state) -> Matcher route (WithRouter route state) -> Router route (WithRouter route state)
constructor config matcher =
  let
    (RouterConfig c) = config
    config' = RouterConfig <| { c | routeConfig = matcher.getConfig}
  in {
    config = config'
  , address = address
  , bindForward = bindForward config' matcher
  , buildUrl = buildUrl config' matcher
  , forward = forward config' matcher
  , redirect = redirect config' matcher
  , match = matchRoute matcher
  }

{-| Launches the router -}
runRouter : RouterConfig route (WithRouter route state) -> RouterResult (WithRouter route state)
runRouter config =
  let
    (RouterConfig c) = config
    matcher = Matcher.matcher config
    router = constructor config matcher
    deps = dependencies router matcher

    initial = c.init
    pathSignal = if c.html5
      then History.path
      else Signal.map (\hash -> Maybe.withDefault "/" <| Maybe.map snd <| String.uncons hash) History.hash

    init = Signal.map (singleton << (,) True << setUrl deps)
      <| if c.removeTrailingSlash then Signal.map Matcher.removeTrailingSlash pathSignal else pathSignal

    -- inputs : Signal (List (Bool, Action state))
    inputs =
      List.foldl (Signal.Extra.fairMerge List.append)
      init <|
      (Signal.map (List.map ((,) False)) mailbox.signal) -- actions from events
      :: List.map (Signal.map (singleton << (,) True))  c.inits
      ++ List.map (Signal.map (singleton << (,) False)) c.inputs

    -- update : List (Bool, Action state) -> (state, ActionEffects state) -> (state, ActionEffects state)
    update  actions (state,_) = List.foldl runAction (noFx state)
      <| List.map snd actions

    -- update' : List (Bool, Action state) -> (state, ActionEffects state)
    update' actions = List.foldl runAction (noFx initial)
      <| List.map snd
      <| List.filter fst actions

    result = Signal.Extra.foldp' update update' inputs
    state = Signal.dropRepeats <| Signal.map fst result
    render' = \state -> render router (List.map deps.getHandlers << matcher.traverse) state
  in
    {
      html  = Signal.map render' state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }
