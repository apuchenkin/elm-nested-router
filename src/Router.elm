module Router exposing ( runRouter, initialState )
{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) ([Live demo](http://apuchenkin.github.io/elm-nested-router/example))
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details

@docs runRouter, initialState
-}

import Dict
import String
import Navigation
import Html             exposing (Html)
-- import Task             exposing (Task)
import Html.Events      exposing (onWithOptions)
-- import Html.App         as App
import Html.Attributes  as Attr
import Json.Decode      as Json

import Router.Matcher      as Matcher exposing (Matcher)
-- import Router.Helpers      exposing (..)
import Router.Types        exposing (..)
import Router.Functions    exposing (..)

hash : Char
hash = '#'

{-| Initial state for router. Fed this into your application state -}
initialState : RouterState route
initialState = {
    route = Nothing
  , params = Dict.empty
  }

{-| binds forward action to existing HTML attributes. Exposed by `Router` -}
bindForward : RouterConfig flags route state -> Matcher flags route state -> Route route -> List (Html.Attribute (Action state)) -> List (Html.Attribute (Action state))
bindForward config matcher route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    action = forward config matcher route
  in
    Attr.href (buildUrl config matcher route)
    :: onWithOptions "click" options (Json.succeed action)
    :: attrs

{-| Decomposes Route to string. Exposed by `Router` -}
buildUrl : RouterConfig flags route state -> Matcher flags route state -> Route route -> String
buildUrl routerConfig matcher (route, params) =
  let
    (RouterConfig config) = routerConfig
    url = matcher.buildUrl (route, params)
    url' = if config.removeTrailingSlash then Matcher.removeTrailingSlash url else url
    url'' = if config.html5 then url' else String.cons hash url'
  in url''

{-| Preforms a transition to provided `Route`. Exposed by `Router` -}
forward : RouterConfig flags route state -> Matcher flags route state -> Route route -> Action state
forward routerConfig matcher route state =
  let
    -- (RouterConfig config) = routerConfig
    url = buildUrl routerConfig matcher route
    msg = Navigation.newUrl url
  in Response (state, msg)

{-| Redirects to provided `Route`. Exposed by `Router` -}
redirect : RouterConfig flags route state -> Matcher flags route state -> Route route -> Action state
redirect routerConfig matcher route state =
  let
    -- (RouterConfig config) = routerConfig
    url = buildUrl routerConfig matcher route
    msg = Navigation.modifyUrl url
  in Response (state, msg)

{-| @Private
  Preforms attempt to match provided url to a route by a given routes configuration
  -}
matchRoute : Matcher flags route state -> String -> Maybe (Route route)
matchRoute matcher url = matcher.match url

{-| Router constructor -}
constructor : RouterConfig flags route state -> Matcher flags route state -> Router flags route state
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

update : (Action state) -> state -> (state, Cmd (Action state))
update action state = let
    (Response state') = action state
  in state'


{-| Launches the router -}
runRouter : RouterConfig flags route (WithRouter route state) -> Program flags -- flags
--RouterResult (WithRouter route state)
runRouter config =
  let
    (RouterConfig c) = config
    matcher = Matcher.matcher config
    router = constructor config matcher
    deps = dependencies router matcher

    --
    -- initial = c.init
    -- pathSignal = if c.html5
    --   then History.path
    --   else Signal.map (\hash -> Maybe.withDefault "/" <| Maybe.map snd <| String.uncons hash) History.hash
    --
    -- init = Signal.map (singleton << (,) True << setUrl deps)
    --   <| if c.removeTrailingSlash then Signal.map Matcher.removeTrailingSlash pathSignal else pathSignal
    --
    -- -- inputs : Signal (List (Bool, Action state))
    -- inputs =
    --   List.foldl (Signal.Extra.fairMerge List.append)
    --   init <|
    --   (Signal.map (List.map ((,) False)) mailbox.signal) -- actions from events
    --   :: List.map (Signal.map (singleton << (,) True))  c.inits
    --   ++ List.map (Signal.map (singleton << (,) False)) c.inputs
    --
    -- -- update : List (Bool, Action state) -> (state, ActionEffects state) -> (state, ActionEffects state)
    -- update  actions (state,_) = List.foldl runAction (noFx state)
    --   <| List.map snd actions
    --
    -- -- update' : List (Bool, Action state) -> (state, ActionEffects state)
    -- update' actions = List.foldl runAction (noFx initial)
    --   <| List.map snd
    --   <| List.filter fst actions
    --
    -- result = Signal.Extra.foldp' update update' inputs
    -- state = Signal.dropRepeats <| Signal.map fst result
    render' state = render router (List.map deps.getHandlers << matcher.traverse) state

    -- urlUpdate : Maybe route -> (WithRouter route state) -> ((WithRouter route state), Cmd (Action (WithRouter route state)))
    urlUpdate maybeRoute state =
      let
        _ = Debug.log "urlUpdate" maybeRoute
      in case maybeRoute of
        Nothing -> update (c.fallbackAction deps.router) state
        Just route -> update (setRoute deps route) state

    dehash hash = Maybe.withDefault "/" <| Maybe.map snd <| String.uncons hash
    parser = Navigation.makeParser (matcher.match << dehash << .hash)
  in
    Navigation.programWithFlags parser
    {
      init = c.init
    , update = update
    , urlUpdate = urlUpdate
    , view = render'
    , subscriptions = \_ -> Sub.none
    }


    --
    -- {
    --   html  = Signal.map render' state
    -- , state = state
    -- , tasks = Signal.map (Effects.toTask mailbox.address << snd) result
    -- }
