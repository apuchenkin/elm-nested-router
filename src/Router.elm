module Router (router, runRouter, initialState) where

import Dict
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

-- import Response as R
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  , cache = {unwrap = Dict.empty, rawUrl = Dict.empty, traverse = Dict.empty}
  }

-- @public
-- binds forward action to existing HTML attributes
bindForward : RouterConfig route (WithRouter route state) -> RouterCache route -> Route route -> List Html.Attribute -> List Html.Attribute
bindForward config cache route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    action _ = Signal.message address <| forward config route
  in
    Attr.href (buildUrl config cache route)
    :: onWithOptions "click" options Json.value action
    :: attrs

-- decompose Route to string
buildUrl : RouterConfig route (WithRouter route state) -> RouterCache route -> Route route -> String
buildUrl config cache (route, params) =
  let
  raw =  case Dict.get (toString route) cache.rawUrl of
    Just value -> value
    Nothing -> Matcher.composeRawUrl (.segment << config.config) config.routes route

  raws = case Dict.get raw cache.unwrap of
    Just value -> value
    Nothing -> Matcher.unwrap raw

  in Matcher.buildRawUrl raws (route, params) -- Lib.Matcher.combineParams state.params

forward : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
forward config route state =
  let
    url   = buildUrl config state.router.cache route
    task  = History.setPath url |> Task.map (always (\s -> Response <| noFx s))
  in Response (state, Effects.task task)

redirect : RouterConfig route (WithRouter route state) -> Route route -> Action (WithRouter route state)
redirect config route state =
  let
    url   = buildUrl config state.router.cache route
    task  = History.replacePath url |> Task.map (always (\s -> Response <| noFx s))
  in Response (state, Effects.task task)

{-| Router constructor -}
router : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
router config =
  let
    cache = prepareCache (.segment << config.config) config.routes
    state = config.init
    state' = if config.useCache
      then let rs = state.router in {state | router = {rs | cache = cache}}
      else state

  in Router {
    config        = {config | init = state'}
  , bindForward   = bindForward   config state'.router.cache
  , buildUrl      = buildUrl      config state'.router.cache
  , forward       = forward       config
  , redirect      = redirect      config
  }

runRouter : Router route (WithRouter route state) -> RouterResult (WithRouter route state)
runRouter router =
  let
    (Router r) = router
    initialState = r.config.init
    init = (Signal.map (singleton << (,) True << setUrl router initialState.router.cache) History.path)

    -- inputs : Signal (List (Bool, Action state))
    inputs =
      List.foldl (Signal.Extra.fairMerge List.append)
      init <|
      (Signal.map (List.map ((,) False)) mailbox.signal) -- actions from events
      :: List.map (Signal.map (singleton << (,) True))  r.config.inits
      ++ List.map (Signal.map (singleton << (,) False)) r.config.inputs

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
      html  = Signal.map (render router r.config.fallbackHtml) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }
