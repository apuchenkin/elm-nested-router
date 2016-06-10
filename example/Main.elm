import Dict     exposing (Dict)

import App.Routes exposing (..)
import App.Handlers exposing (..)
import App.Actions exposing (State)
import App.Layout exposing (..)

import Router
import Router.Types  exposing (Router, RouterConfig (..), RouteConfig, Constraint (..))
import Router.Helpers exposing (doNothing)

config : Route -> RouteConfig flags Route State
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

-- init: flags -> Maybe (Route route) -> (state, Cmd (Action state))
main : Program Never
main = Router.runRouter <| RouterConfig {
    init = \_ _ -> (initialState, Cmd.none)
  , html5 = False
  , removeTrailingSlash = True
  , onTransition = \_ _ _ -> doNothing
  , fallbackAction = \_ -> doNothing
  , layout = layout
  , routes = routes
  , routeConfig = config
  }
