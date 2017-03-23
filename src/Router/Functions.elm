module Router.Functions exposing (..)

import Dict
import Html             exposing (Html)

import URL.Route as Route exposing (Route)
import URL.Matcher as Matcher exposing (URL)
import URL.Utils as Utils

import Router.Types exposing (..)
import Router.Helpers exposing (foldActions)
import Router.Navigation exposing (..)

{-| @Private
  Renders handlers for current route
 -}
render :
  Router route (WithRouter route state) msg ->
  WithRouter route state ->
  Html (Msg route msg)
render router state =
    let
      (RouterConfig config) = router.config
      route = state.router.route
      handlers = Maybe.withDefault [] <| Maybe.map ((List.map config.routeConfig) << (Route.traverse (.route << config.routeConfig) config.routes)) route
      views       = List.map (\h -> h.render router) handlers
      htmlParts   = List.foldr (\view parsed -> Dict.union parsed <| view state parsed) Dict.empty views
    in config.layout router state htmlParts

update :
  Router route (WithRouter route state) msg ->
  Msg route msg ->
  Action (WithRouter route state) (Msg route msg)
update router msg =
  let
    (RouterConfig config) = router.config
    updateAction = transition router << (Matcher.match (.route << config.routeConfig) config.routes) << getPath router.config
  in case msg of
    Transition location -> updateAction location
    Forward route -> \state -> (state, forward router.config route)
    Redirect route -> \state -> (state, redirect router.config route)
    AppMsg appMsg -> config.update appMsg

{-| @Private
  Sets provided route ro the state and return state transition from previous route to new one
-}
transition :
  Router route (WithRouter route state) msg ->
  Maybe (Route route) ->
  Action (WithRouter route state) (Msg route msg)
transition router to state =
  let
    (RouterConfig config) = router.config
    rs = state.router
    toParams = Maybe.withDefault Dict.empty <| Maybe.map .arguments to
    from  = Maybe.map (\r -> Route.route r rs.arguments) rs.route
    state_new = { state | router = { rs | route = Maybe.map .route to, arguments = toParams }}

    diff = Maybe.withDefault [] <| Maybe.map (Utils.routeDiff (.route << config.routeConfig) config.routes from) to
    handlers = List.map config.routeConfig diff
    onTransition = config.onTransition router from to
    msgs  = onTransition ++ (List.concat <| List.map .actions handlers)
    actions = List.map config.update msgs
  in
    foldActions actions state_new
