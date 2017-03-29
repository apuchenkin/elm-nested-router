import Dict exposing (Dict)

import App.Routes as Route exposing (routes)
import App.Actions exposing (update, noFx, State, Msg (..))
import App.Layout exposing (layout)
import App.RouteConfig exposing (config)

import Router
import Router.Types exposing (Router, RouterConfig (..), RouteConfig)
import Router.Types as Router
import URL.Route

initialState : State
initialState = {
    router      = Router.initialState
  , categories  = []
  , posts       = []
  , post        = Nothing
  }

-- main : Program Never State (Router.Types.Action State)
main = Router.dispatch
  (noFx initialState)
  (RouterConfig {
    html5 = False
  , removeTrailingSlash = True
  , update = update
  , onTransition = \router _ to ->
    case to of
      Nothing -> [Forward <| URL.Route.route Route.Home Dict.empty]
      Just route -> let
        _ = (Debug.log "onTransition" route)
      in []
  , layout = layout
  , routes = routes
  , routeConfig = config
  , subscriptions = always Sub.none
  })
