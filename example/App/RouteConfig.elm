module App.RouteConfig exposing (..)

import App.Actions exposing (..)
import App.Routes as Route exposing (..)
import App.Handlers exposing (..)
import Router.Types exposing (RouteConfig)
import URL.Segments as Segments exposing ((</>))
import URL.Route exposing ((//>))

config : Route -> RouteConfig Route State Msg
config route = case route of
  Home -> {
    route = Nothing //> Segments.end,
    render = renderHome,
    actions = [LoadCategories]
  }
  NotFound -> {
    route = Nothing //> Segments.static "404",
    render = notFound,
    actions = []
  }
  Static page -> {
    route = Nothing //> Segments.static page,
    render = renderStatic page,
    actions = []
  }
  Category -> {
    route = Just Home //> Segments.enum "category" ["animals", "flowers", "colors"] </> Segments.maybe (Segments.string "subcategory"),
    render = renderCategory,
    actions = [LoadPosts]
  }
  Post -> {
    route = Just Route.Category //> Segments.static "post" </> Segments.int "postId",
    render = renderPost,
    actions = [LoadPost]
  }
