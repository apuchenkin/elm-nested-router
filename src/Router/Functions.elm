module Router.Functions where

import Dict
import List.Extra
import Effects          exposing (Effects)
import Html             exposing (Html)
import MultiwayTreeUtil   exposing (flatten)
import MultiwayTree exposing (Forest)

import Router.Matcher      as Matcher
import Router.Types        exposing (..)
import Router.Helpers      exposing (..)

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
  Renders handlers for current route
 -}
render : Router route (WithRouter route state) -> (WithRouter route state) -> Html
render router state =
    let
      (RouterConfig config) = router.config
      layout      = config.layout
      route       = state.router.route
      handlers    = Maybe.withDefault []
         <| flip Maybe.map route
         <| \r -> getHandlers router Nothing (r, Dict.empty)

      views     = List.map .view handlers
      htmlParts = List.foldr (\view parsed -> Dict.union parsed <| view state parsed) Dict.empty views
    in layout router state htmlParts

{-| @Private
  Performs attempt to match provided url, returns fallback action on fail
  -}
setUrl : Router route (WithRouter route state) -> String -> Action (WithRouter route state)
setUrl router url =
  let
    (RouterConfig config) = router.config
  in case (matchRoute config.routes router.matcher url) of
    Nothing               -> router.redirect config.fallback
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
    (RouterConfig config) = router.config
    handlers = getHandlers router from to
    actions  =
      (config.onTransition router from to)
      :: List.map (combineActions << .actions) handlers

  in Response <| List.foldl runAction (noFx state) actions

{-| @Private
  Returns a set of handlers applicable to transtition between "from" and "to" routes.
-}
getHandlers : Router route state -> Maybe (Route route) -> Route route -> List (Handler state)
getHandlers router from to =
  let
    getConfig = router.matcher.getConfig
    fromRoute = Maybe.map fst from
    fromParams = Maybe.withDefault Dict.empty <| Maybe.map snd from
    toRoute = fst to
    toParams = snd to

    fromPath = Maybe.withDefault [] <| Maybe.map (router.matcher.getPath) fromRoute
    toPath = router.matcher.getPath toRoute
    path = List.map2 (,) fromPath toPath

    fromPath' = Matcher.mapParams (.segment << getConfig) fromPath fromParams
    toPath'   = Matcher.mapParams (.segment << getConfig) toPath toParams

    commons = List.length
      <| List.Extra.takeWhile (uncurry (==))
      <| List.map2 (,) fromPath' toPath'

    routes = List.drop commons toPath

  in List.map (.handler << router.matcher.getConfig) routes

{-| @Private
  Preforms attempt to match provided url to a route by a given routes configuration
  -}
matchRoute : Forest route -> Matcher route state -> String -> Maybe (Route route)
matchRoute routes matcher url =
  let
    routeConfig = matcher.getConfig
    getSegment = .segment << routeConfig
    getConstraints = .constraints << routeConfig
    rawRoute route = (matcher.unwrap (getSegment route), getConstraints route)
  in
    Matcher.matchRaw rawRoute routes url

-- TODO: refactor for better readability
matcher : RouterConfig route state -> Matcher route state
matcher config =
  let
    (RouterConfig c) = config
    getSegment = .segment << c.routeConfig
    forest = c.routes
    routes = List.concat <| List.map flatten forest
    urls = List.map composeRawUrl'' routes
    sids = List.map toString routes
    dict = Dict.fromList <| List.map2 (,) sids routes
    stringToRoute sid = case Dict.get sid dict of
      Just route -> route
      Nothing -> Debug.crash <| "stringToRoute: " ++ sid
    segments = List.map getSegment routes
    composeRawUrl' = memoFallback (\sid -> Matcher.composeRawUrl getSegment forest (stringToRoute sid)) sids
    composeRawUrl'' route = composeRawUrl' (toString route)
    getPath' = memoFallback (\sid -> Matcher.getPath (stringToRoute sid) forest) sids
    getConfig = memoFallback (\sid -> c.routeConfig (stringToRoute sid)) sids
    getConfig' route = getConfig (toString route)
  in
    {
      unwrap = memoFallback Matcher.unwrap (segments ++ urls)
    , composeRawUrl = composeRawUrl''
    , getPath = (\route -> getPath' (toString route))
    , getConfig = getConfig'
    }
