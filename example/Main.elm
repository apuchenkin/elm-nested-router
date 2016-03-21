import Task     exposing (Task)
import Html     exposing (Html)
import Effects  exposing (Never)
import Dict     exposing (Dict)

import App.Routes exposing (..)
import App.Handlers exposing (..)
import App.Actions exposing (State)
import App.Layout exposing (..)

import Router
import Router.Types  exposing (Router, RouterConfig (..), RouteConfig, RouterResult, Constraint (..))
import Router.Helpers exposing (doNothing)

config : Route -> RouteConfig Route State
config route = case route of
  Home -> {
      segment = "/",
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
      segment = ":category[/:subcategory]",
      bypass = False,
      parent = Just Home,
      constraints = Dict.fromList [("category", Enum ["animals", "flowers", "colors"])],
      handler = categoryHandler
    }
  Post -> {
      segment = "/post/:postId",
      bypass = False,
      parent = Just Category,
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

result : RouterResult State
result = Router.runRouter <| RouterConfig {
    init      = initialState
  , html5     = False
  , removeTrailingSlash = True
  , onTransition = \_ _ _ -> doNothing
  , fallback  = (NotFound, Dict.empty)
  , layout    = layout
  , routes    = routes
  , routeConfig  = config
  , inits     = []
  , inputs    = []
  }

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks
