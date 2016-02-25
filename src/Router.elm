module Router (router, runRouter, initialState) where
{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example)
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details
([Live demo](http://apuchenkin.github.io/elm-nested-router/example))

@docs router, runRouter, initialState
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

import Router.Matcher      as Matcher
import Router.Helpers      exposing (..)
import Router.Types        exposing (..)
import Router.Functions    exposing (..)
import Router.Mailbox      exposing (..)

hash : Char
hash = '#'

{-| Initial state for router. Fed this intou your application state -}
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  , cache = {unwrap = Dict.empty, rawUrl = Dict.empty, traverse = Dict.empty}
  }

{-| binds forward action to existing HTML attributes. Exposed by `Router` -}
bindForward : RouterConfig route (WithRouter route state) -> RouterCache route -> Route route -> List Html.Attribute -> List Html.Attribute
bindForward config cache route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    action _ = Signal.message address <| forward config route
  in
    Attr.href (buildUrl config cache route)
    :: onWithOptions "click" options Json.value action
    :: attrs

{-| Decomposes Route to string. Exposed by `Router` -}
buildUrl : RouterConfig route (WithRouter route state) -> RouterCache route -> Route route -> String
buildUrl routerConfig cache (route, params) =
  let
    (RouterConfig config) = routerConfig
    raw =  case Dict.get (toString route) cache.rawUrl of
      Just value -> value
      Nothing -> Matcher.composeRawUrl (.segment << config.routeConfig) config.routes route

    raws = case Dict.get raw cache.unwrap of
      Just value -> value
      Nothing -> Matcher.unwrap raw

  in Matcher.buildRawUrl raws (route, params)

{-| Preforms a transition to provided `Route`. Exposed by `Router` -}
forward : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
forward routerConfig route state =
  let
    (RouterConfig config) = routerConfig
    url   = buildUrl routerConfig state.router.cache route
    url'  = if config.html5 then url else String.cons hash url
    task  = History.setPath url' |> Task.map (always doNothing)
  in Response (state, Effects.task task)

{-| Redirects to provided `Route`. Exposed by `Router` -}
redirect : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
redirect routerConfig route state =
  let
    (RouterConfig config) = routerConfig
    url   = buildUrl routerConfig state.router.cache route
    url'  = if config.html5 then url else String.cons hash url
    task  = History.replacePath url' |> Task.map (always doNothing)
  in Response (state, Effects.task task)

{-| Router constructor -}
router : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
router (RouterConfig config) =
  let
    cache = prepareCache (.segment << config.routeConfig) config.routes
    state = config.init
    state' = if config.useCache
      then let rs = state.router in {state | router = {rs | cache = cache}}
      else state
    config' = RouterConfig {config | init = state'}

  in {
    config        = config'
  , bindForward   = bindForward   config' state'.router.cache
  , buildUrl      = buildUrl      config' state'.router.cache
  , forward       = forward       config'
  , redirect      = redirect      config'
  }

{-| Launches the router -}
runRouter : Router route (WithRouter route state) -> RouterResult (WithRouter route state)
runRouter router =
  let
    (RouterConfig config) = router.config
    initialState = config.init
    pathSignal = if config.html5
      then History.path
      else Signal.map (\hash -> Maybe.withDefault "/" <| Maybe.map snd <| String.uncons hash) History.hash

    init = Signal.map (singleton << (,) True << setUrl router initialState.router.cache) pathSignal

    -- inputs : Signal (List (Bool, Action state))
    inputs =
      List.foldl (Signal.Extra.fairMerge List.append)
      init <|
      (Signal.map (List.map ((,) False)) mailbox.signal) -- actions from events
      :: List.map (Signal.map (singleton << (,) True))  config.inits
      ++ List.map (Signal.map (singleton << (,) False)) config.inputs

    -- update : List (Bool, Action state) -> (state, ActionEffects state) -> (state, ActionEffects state)
    update  actions (state,_) = List.foldl runAction (noFx state)
      <| List.map snd actions

    -- update' : List (Bool, Action state) -> (state, ActionEffects state)
    update' actions           = List.foldl runAction (noFx initialState)
      <| List.map snd
      <| List.filter fst actions

    result = foldp' update update' inputs
    state = Signal.map fst result
  in
    {
      html  = Signal.map (render router) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }
