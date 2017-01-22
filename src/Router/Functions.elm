module Router.Functions exposing (..)

import Dict
import Html             exposing (Html)

import Router.Helpers      exposing (foldActions)
import Router.Matcher      as Matcher exposing (Matcher)
import Router.Types        exposing (..)
import Router.Navigation   exposing (..)

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

update :
  Router route (WithRouter route state) msg ->
  Matcher route (WithRouter route state) msg ->
  (route -> Handler route (WithRouter route state) msg) ->
  (Msg route msg) -> Action (WithRouter route state) (Msg route msg)
update router matcher getHandlers msg =
  let
    (RouterConfig config) = router.config
    urlUpdate =  transition router matcher getHandlers
    updateAction = urlUpdate << matcher.match << getPath router.config

  in case msg of
    Transition location -> updateAction location
    Forward route -> \state -> (state, forward router.config matcher route)
    Redirect route -> \state -> (state, redirect router.config matcher route)
    AppMsg appMsg -> config.update appMsg

{-| @Private
  Sets provided route ro the state and return state transition from previous route to new one
-}
transition :
  Router route (WithRouter route state) msg ->
  Matcher route (WithRouter route state) msg ->
  (route -> Handler route (WithRouter route state) msg) ->
  Maybe (Route route) ->
  Action (WithRouter route state) (Msg route msg)
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
    onTransition = config.onTransition router from to
    msgs  = onTransition ++ (List.concat <| List.map .actions handlers)
    actions = List.map config.update msgs

  in foldActions actions state_new

createHandlers :
    Router route (WithRouter route state) msg ->
    Matcher route (WithRouter route state) msg ->
    (route -> Handler route (WithRouter route state) msg)
createHandlers router matcher =
    let getHandlers = Matcher.memoFallback (\sid -> ((\h -> h router) << .handler << matcher.getConfig) (matcher.stringToRoute sid)) matcher.sids
    in getHandlers << toString
