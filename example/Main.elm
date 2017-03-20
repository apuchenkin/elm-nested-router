import Dict     exposing (Dict)

import App.Routes as Route exposing (..)
import App.Handlers exposing (..)
import App.Actions exposing (..)
import App.Layout exposing (..)
import App.RouteConfig exposing (..)

import Router
import Router.Types  exposing (Router, RouterConfig (..), RouteConfig)
import Router.Helpers exposing (noFx)
import Router.Types as Router
import Router.Functions as Functions

initialState : State
initialState = {
    router      = Functions.initialState
  , categories  = []
  , posts       = []
  , post        = Nothing
  }

update msg = case msg of
  LoadCategories -> loadCategories
  LoadPosts -> loadPosts
  LoadPost -> loadPost
  UpdateCategories categories  -> updateCategories categories
  UpdatePosts posts -> updatePosts posts
  UpdatePost post -> updatePost post

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
