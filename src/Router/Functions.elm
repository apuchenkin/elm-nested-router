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
render : Router route (WithRouter route state) -> (route -> List (Handler (WithRouter route state))) -> (WithRouter route state) -> Html
render router getHandlers state =
    let
      (RouterConfig config) = router.config
      route       = state.router.route
      handlers    = Maybe.withDefault [] <| Maybe.map getHandlers route
      views       = List.map .view handlers
      htmlParts   = List.foldr (\view parsed -> Dict.union parsed <| view state parsed) Dict.empty views
    in config.layout router state htmlParts

{-| @Private
  Performs attempt to match provided url, returns fallback action on fail
  -}
setUrl : Router route (WithRouter route state) -> (route -> Handler (WithRouter route state)) -> String -> Action (WithRouter route state)
setUrl router getHandler url =
  let
    (RouterConfig config) = router.config
  in case router.match url of
    Nothing               -> router.redirect config.fallback
    Just route            -> setRoute router getHandler route

{-| @Private
  Sets provided route ro the state and return state transition from previous route to new one
-}
setRoute : Router route (WithRouter route state) -> (route -> Handler (WithRouter route state)) -> Route route -> Action (WithRouter route state)
setRoute router getHandler route state =
  let
    rs = state.router
    (toRoute, toParams) = route
    from  = Maybe.map (\r -> (r, rs.params)) rs.route
    state' = { state | router = { rs | route = Just toRoute, params = toParams }}
  in
    transition router getHandler from route state'

{-| @Private
  A composite transition action between "from" and "to" routes
  Resulting action is composed from handlers, applicable for transistion
-}
transition : Router route (WithRouter route state) -> (route -> Handler (WithRouter route state)) -> Transition route (WithRouter route state)
transition router getHandler from to state =
  let
    (RouterConfig config) = router.config
    diff = Matcher.routeDiff config.routeConfig from to
    handlers = List.map getHandler diff
    actions  =
      (config.onTransition router from to)
      :: List.map (combineActions << .actions) handlers

  in Response <| List.foldl runAction (noFx state) actions
