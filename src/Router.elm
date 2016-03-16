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
    raws = matcher.unwrap <| matcher.composeRawUrl route
    url = Matcher.buildRawUrl raws (route, params)
    url' = if config.html5 then url else String.cons hash url
    url'' = if config.removeTrailingSlash then Matcher.removeTrailingSlash url else url
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

{-| Router constructor -}
router : RouterConfig route (WithRouter route state) -> Router route (WithRouter route state)
router config =
  let
    matcher' = matcher config
  in {
    config        = config
  , address       = address
  , matcher       = matcher'
  , bindForward   = bindForward   config matcher'
  , buildUrl      = buildUrl      config matcher'
  , forward       = forward       config matcher'
  , redirect      = redirect      config matcher'
  }

{-| Launches the router -}
runRouter : Router route (WithRouter route state) -> RouterResult (WithRouter route state)
runRouter router =
  let
    (RouterConfig config) = router.config
    initial = config.init
    pathSignal = if config.html5
      then History.path
      else Signal.map (\hash -> Maybe.withDefault "/" <| Maybe.map snd <| String.uncons hash) History.hash

    init = Signal.map (singleton << (,) True << setUrl router)
      <| if config.removeTrailingSlash then Signal.map Matcher.removeTrailingSlash pathSignal else pathSignal

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
    update' actions = List.foldl runAction (noFx initial)
      <| List.map snd
      <| List.filter fst actions

    result = Signal.Extra.foldp' update update' inputs
    state = Signal.map fst result
  in
    {
      html  = Signal.map (render router) state
    , state = state
    , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    }
