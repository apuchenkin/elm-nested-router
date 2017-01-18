module Router.Functions exposing (..)

import Dict
import Html             exposing (Html)

-- import Router.Helpers      exposing (noFx)
import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)
-- import Navigation


-- runAction : Action state -> state -> (state, Cmd msg)
-- runAction action state =
--   let
--     (Response res) = action state
--   in res

{-| @Private
  Renders handlers for current route
 -}
render :
  Router route (WithRouter route state) msg
  -> (route -> List (Handler route (WithRouter route state) msg))
  -> WithRouter route state -> Html (Msg route msg)
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
  Router route (WithRouter route state) msg ->
  Matcher route (WithRouter route state) msg ->
  (route -> Handler route (WithRouter route state) msg) ->
  Maybe (Route route) -> (WithRouter route state) -> (WithRouter route state, Cmd (Msg route msg))
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
    -- onTransition = config.transition router from to
    msgs  = List.concat <| List.map .actions handlers

  in List.foldl (\msg (s, c) ->
    let (s_, c_) = config.update msg s
    in (s_, Cmd.batch [c, c_])
    ) (state_new, Cmd.none) msgs
    -- state_new ! (onTransition :: actions)

createHandlers :
    Router route (WithRouter route state) msg ->
    Matcher route (WithRouter route state) msg ->
    (route -> Handler route (WithRouter route state) msg)
createHandlers router matcher =
    let getHandlers = Matcher.memoFallback (\sid -> ((\h -> h router) << .handler << matcher.getConfig) (matcher.stringToRoute sid)) matcher.sids
    in getHandlers << toString
