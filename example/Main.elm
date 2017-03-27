import Dict exposing (Dict)

import App.Routes as Route exposing (routes)
-- import App.Handlers exposing (..)
import App.Actions exposing (update)
import App.Layout exposing (layout)
-- import App.RouteConfig exposing (..)

import Router
import Router.Types exposing (Router, RouterConfig (..), RouteConfig)
import Router.Types as Router

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
  , onTransition = \r _ to -> []
    -- case to of
    --   Nothing -> r.redirect (Home, Dict.empty)
    --   Just rr -> let
    --     _ = (Debug.log "onTransition" rr)
    --   in doNothing
  , layout = layout
  , routes = routes
  , routeConfig = config
  , subscriptions = always Sub.none
  })
