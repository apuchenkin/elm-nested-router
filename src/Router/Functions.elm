module Router.Functions exposing (..)

import Dict
import Html             exposing (Html)

import Router.Helpers      exposing (combineActions, noFx)
import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)

runAction : Action state -> state -> (state, Commands state)
runAction action state =
  let
    (Response res) = action state
  in res

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
    toRoute = Maybe.map Tuple.first to
    toParams = Maybe.withDefault Dict.empty <| Maybe.map Tuple.second to
    from  = Maybe.map (\r -> (r, rs.params)) rs.route
    state_new = { state | router = { rs | route = toRoute, params = toParams }}

    diff = Maybe.withDefault [] <| Maybe.map (Matcher.routeDiff matcher from) to
    handlers = List.map getHandlers diff
    onTransition = config.transition router from to
    actions  = List.map (combineActions << .actions) handlers
  in
    combineActions (onTransition :: actions) state_new

createHandlers :
    Router route (WithRouter route state) ->
    Matcher route (WithRouter route state) ->
    (route -> Handler (WithRouter route state))
createHandlers router matcher =
    let getHandlers = Matcher.memoFallback (\sid -> ((\h -> h router) << .handler << matcher.getConfig) (matcher.stringToRoute sid)) matcher.sids
    in getHandlers << toString
