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

config : Route -> RouteConfig Route State
config route = case route of
  Home -> {
      segment = "/",
      constraints = Dict.empty,
      handler = homeHandler
    }
  NotFound -> {
      segment = "/404",
      constraints = Dict.empty,
      handler = notFoundHandler
    }
  Static page -> {
      segment = "/" ++ page,
      constraints = Dict.empty,
      handler = staticHandler page
    }
  Category -> {
      segment = ":category[/:subcategory]",
      constraints = Dict.fromList [("category", Enum ["animals", "flowers", "colors"])],
      handler = categoryHandler
    }
  Post -> {
      segment = "/post/:postId",
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

router : Router Route State
router = Router.router <| RouterConfig {
    init      = initialState
  , useCache  = False
  , html5     = False
  , fallback  = (NotFound, Dict.empty)
  , layout    = layout
  , routes    = routes
  , routeConfig  = config
  , inits     = []
  , inputs    = []
  }

result : RouterResult State
result = Router.runRouter router

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks
