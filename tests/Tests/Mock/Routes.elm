module Tests.Mock.Routes exposing (..)

import Matcher.Matcher as Matcher
import Matcher.Segments as Segments exposing (..)

type Route = Home | Category String | Post | Article String

routeConfig : Matcher.GetConfig Route
routeConfig route = case route of
  Home -> {
    parent = Nothing
  , segment = end
  }
  Category "bear" -> {
    parent = Just Home
  , segment = static "category" </> static "bear"
  }
  Category "tiger" -> {
    parent = Just Home
  , segment = static "category" </> static "tiger"
  }
  Category category -> {
    parent = Just Home
  , segment = static "category" </> string "category" </> maybe (string "subcategory")
  }
  Post -> {
    parent = Just (Category "bear")
  , segment = static "post" </> int "post"
  }
  Article category -> {
    parent = Just <| Category category
  , segment = static "article" </> enum "animal" ["lion", "penguin"]
  }

routes : List Route
routes = [Home, Category "bear", Category "tiger", Category "animal", Post, Article "animal"]
