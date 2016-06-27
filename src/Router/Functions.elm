module Router.Functions exposing (..)

import Dict
import Html             exposing (Html)

import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)
import Router.Helpers      exposing (..)

{-| @Private
  Runs the action for the specified state and initial effects
 -}
runAction : Action state -> (state, ActionEffects state) -> (state, ActionEffects state)
runAction action (state, effects) =
    let
      (Response (state', effects')) = action state
    in
      (state', Cmd.batch [effects, effects'])

{-| @Private
  Folds actions for a handlers into a single action
-}
combineActions : List (Action state) -> Action state
combineActions actions = \state -> Response <| List.foldl runAction (noFx state) actions

{-| @Private
  Renders handlers for current route
 -}
render :
  Router route (WithRouter route state)
  -> (route -> List (Handler (WithRouter route state)))
  -> WithRouter route state -> Html (Action (WithRouter route state))
render router getHandlers state =
    let
      (RouterConfig config) = router.config
      route       = state.router.route
      handlers    = Maybe.withDefault [] <| Maybe.map getHandlers route
      views       = List.map .view handlers
      htmlParts   = List.foldr (\view parsed -> Dict.union parsed <| view state parsed) Dict.empty views
    in config.layout router state htmlParts

{-| @Private
  Sets provided route ro the state and return state transition from previous route to new one
-}
transition :
  Router route (WithRouter route state) ->
  Matcher route (WithRouter route state) ->
  (route -> Handler (WithRouter route state)) ->
  Maybe (Route route) -> Action (WithRouter route state)
transition router matcher getHandlers to state =
  let
    (RouterConfig config) = router.config
    rs = state.router
    toRoute = Maybe.map fst to
    toParams = Maybe.withDefault Dict.empty <| Maybe.map snd to
    from  = Maybe.map (\r -> (r, rs.params)) rs.route
    state' = { state | router = { rs | route = toRoute, params = toParams }}

    diff = Maybe.withDefault [] <| Maybe.map (Matcher.routeDiff matcher from) to
    handlers = List.map getHandlers diff
    onTransition = config.transition router from to
    actions  = List.map (combineActions << .actions) handlers
  in
    combineActions (onTransition :: actions) state'

createHandlers :
    Router route (WithRouter route state) ->
    Matcher route (WithRouter route state) ->
    (route -> Handler (WithRouter route state))
createHandlers router matcher =
    let getHandlers = Router.Helpers.memoFallback (\sid -> ((\h -> h router) << .handler << matcher.getConfig) (matcher.stringToRoute sid)) matcher.sids
    in getHandlers << toString
