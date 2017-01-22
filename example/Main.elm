import Dict     exposing (Dict)

import App.Routes as Route exposing (..)
import App.Handlers exposing (..)
import App.Actions exposing (..)
import App.Layout exposing (..)

import Router
import Router.Types  exposing (Router, RouterConfig (..), RouteConfig, Constraint (..))
import Router.Helpers exposing (noFx)
import Router.Types as Router

config : Route -> RouteConfig Route State Msg
config route = case route of
  Home -> {
      segment = "",
      bypass = False,
      parent = Nothing,
      constraints = Dict.empty,
      handler = homeHandler
    }
  NotFound -> {
      segment = "/404",
      bypass = False,
      parent = Nothing,
      constraints = Dict.empty,
      handler = notFoundHandler
    }
  Static page -> {
      segment = "/" ++ page,
      bypass = False,
      parent = Nothing,
      constraints = Dict.empty,
      handler = staticHandler page
    }
  Category -> {
      segment = "/:category[/:subcategory]",
      bypass = False,
      parent = Just Home,
      constraints = Dict.fromList [("category", Enum ["animals", "flowers", "colors"])],
      handler = categoryHandler
    }
  Post -> {
      segment = "/post/:postId",
      bypass = False,
      parent = Just Route.Category,
      constraints = Dict.fromList [("postId", Int)],
      handler = postHandler
    }

initialState : State
initialState = {
    router      = Router.initialState
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
