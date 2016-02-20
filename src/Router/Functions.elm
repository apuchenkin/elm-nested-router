module Router.Functions where

import Dict
import List.Extra
import MultiwayTreeUtil
import MultiwayTree     exposing (Forest)
import Effects          exposing (Effects)
import Html             exposing (Html)

import Router.Matcher      as Matcher
import Router.Types        exposing (..)
import Router.Helpers      exposing (..)
import Router.Mailbox      exposing (address)

{-| @Private
  Runs the action for the specified state and initial effects
 -}
runAction : Action state -> (state, ActionEffects state) -> (state, ActionEffects state)
runAction action (state, effects) =
    let (Response (state', effects')) = action state
    in (state', Effects.batch [effects, effects'])

{-| @Private
  Folds actions for a handlers into a single action
-}
combineActions : List (Action state) -> Action state
combineActions actions = \state -> Response <| List.foldl runAction (noFx state) actions

{-| @Private
  Creates cache for a given router config
-}
prepareCache : (route -> RawSegment) -> Forest route -> RouterCache route
prepareCache getSegment forest =
  let
    routes = List.concat <| List.map MultiwayTreeUtil.flatten forest
    urls = flip List.map routes <| \r -> (toString r, Matcher.composeRawUrl getSegment forest r)
    segments = List.map getSegment routes
    unwraps = flip List.map (segments ++ List.map snd urls) <| \url -> (url, Matcher.unwrap url)
    traverses = flip List.map routes (\route -> (toString route, Matcher.getPath route forest))
  in {
    rawUrl    = Dict.fromList urls
  , unwrap    = Dict.fromList unwraps
  , traverse  = Dict.fromList traverses
  }

{-| @Private
  Renders handlers for current route
 -}
render : Router route (WithRouter route state) -> Html -> (WithRouter route state) ->  Html
render router fallback state =
    let
      route     = state.router.route
      handlers  = Maybe.withDefault []
         <| flip Maybe.map route
         <| \r -> getHandlers router state.router.cache Nothing (r, Dict.empty)

      views     = List.map .view handlers
      html      = List.foldr (\view parsed -> view address state parsed) Nothing views
    in Maybe.withDefault fallback html

{-| @Private
  Performs attempt to match provided url, returns fallback action on fail
  -}
setUrl : Router route (WithRouter route state) -> RouterCache route -> String -> Action (WithRouter route state)
setUrl router cache url =
  let (Router r) = router
  in case (matchRoute r.config cache url) of
    Nothing               -> setRoute router r.config.fallback
    Just route            -> setRoute router route

{-| @Private
  Sets provided route ro the state and return state transition from previous route to new one
-}
setRoute : Router route (WithRouter route state) -> Route route -> Action (WithRouter route state)
setRoute router route state =
  let
    rs = state.router
    (toRoute, toParams) = route
    from  = Maybe.map (\r -> (r, rs.params)) rs.route
    state' = { state | router = { rs | route = Just toRoute, params = toParams }}
  in
    transition router from route state'

{-| @Private
  A composite transition action between "from" and "to" routes
  Resulting action is composed from handlers, applicable for transistion
-}
transition : Router route (WithRouter route state) -> Transition route (WithRouter route state)
transition router from to state =
  let
    -- _ = Debug.log "transition: from" (from, to)
    handlers = getHandlers router state.router.cache from to
    actions  = List.map (combineActions << .actions) handlers
  in  Response <| List.foldl runAction (noFx state) actions

{-| @Private
  Returns a set of handlers applicable to transtition between "from" and "to" routes.
-}
getHandlers : Router route state -> RouterCache route -> Maybe (Route route) -> Route route -> List (Handler state)
getHandlers router cache from to =
  let
    (Router r) = router
    fromRoute = Maybe.map fst from
    fromParams = Maybe.withDefault Dict.empty <| Maybe.map snd from
    toRoute = fst to
    toParams = snd to

    fromPath = Maybe.withDefault []
     <| flip Maybe.map fromRoute
     <| \f -> case Dict.get (toString f) cache.traverse of
      Just path -> path
      Nothing   -> Matcher.getPath f r.config.routes
    toPath = case Dict.get (toString toRoute) cache.traverse of
     Just path -> path
     Nothing   -> Matcher.getPath toRoute r.config.routes
    path = List.map2 (,) fromPath toPath

    fromPath' = Matcher.mapParams (.segment << r.config.config) fromPath fromParams
    toPath'   = Matcher.mapParams (.segment << r.config.config) toPath    toParams

    commons = List.length
      <| List.Extra.takeWhile (uncurry (==))
      <| List.map2 (,) fromPath' toPath'

    routes = List.drop commons toPath

  in List.map ((\h -> h router) << .handler << r.config.config) <| routes

{-| @Private
  preforms attempt to match provided url to a route by a given routes configuration
  -}
matchRoute : RouterConfig route state -> RouterCache route -> String -> Maybe (Route route)
matchRoute config cache url =
  let
    rawRoute route = case Dict.get (.segment <| config.config route) cache.unwrap of
      Just value -> (value, .constraints <| config.config route)
      Nothing -> (Matcher.unwrap <| .segment <| config.config route, .constraints <| config.config route)
  in
    Matcher.matchRaw rawRoute config.routes url
