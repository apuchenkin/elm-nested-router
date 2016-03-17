module Router.Functions where

import Dict
import Effects          exposing (Effects)
import Html             exposing (Html)

import Router.Matcher      as Matcher exposing (Matcher)
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
      handlers    = Maybe.withDefault [] <| Maybe.map router.getHandlers route
      views       = List.map .view handlers
      htmlParts   = List.foldr (\view parsed -> Dict.union parsed <| view state parsed) Dict.empty views
    in layout router state htmlParts

{-| @Private
  Performs attempt to match provided url, returns fallback action on fail
  -}
setUrl : Router route (WithRouter route state) -> String -> Action (WithRouter route state)
setUrl router url =
  let
    (RouterConfig config) = router.config
  in case router.match url of
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
    handlers = Matcher.getHandlers config.routeConfig from to
    actions  =
      (config.onTransition router from to)
      :: List.map (combineActions << .actions) handlers

  in Response <| List.foldl runAction (noFx state) actions

{-| @Private
  Preforms attempt to match provided url to a route by a given routes configuration
  -}
matchRoute : Matcher route state -> String -> Maybe (Route route)
matchRoute matcher url = matcher.match url
