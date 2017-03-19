module Tests.Mock.RouteConfig exposing (..)

import Dict
import Html

import Router.Types exposing (GetConfig)
import Matcher.Matcher exposing ((//>))
import Matcher.Segments exposing (..)

import Tests.Mock.Routes exposing (..)
import Tests.Mock.Actions exposing (..)

routeConfig : GetConfig Route State Msg
routeConfig route = case route of
  Home -> {
    route = Nothing //> end
  , render = \_ _ -> Dict.fromList [("home", Html.text "home")]
  , actions = [NoOp]
  }
  Category "bear" -> {
    route = Just Home //> static "category" </> static "bear"
  , render = \_ _ -> Dict.fromList [("category", Html.text "bear")]
  , actions = [Append ""]
  }
  Category "tiger" -> {
    route = Just Home //> static "category" </> static "tiger"
  , render = \_ _ -> Dict.fromList [("category", Html.text "tiger")]
  , actions = [NoOp, Append "tiger"]
  }
  Category category -> {
    route = Just Home //> static "category" </> string "category" </> maybe (string "subcategory")
  , render = \_ _ -> Dict.fromList [("category", Html.text category)]
  , actions = [Append category]
  }
  Post -> {
    route = Just (Category "bear") //> static "post" </> int "post"
  , render = \_ _ -> Dict.fromList [("post", Html.text "post")]
  , actions = [Succ]
  }
  Article category -> {
    route = Just (Category category) //> static "article" </> enum "animal" ["lion", "penguin"]
  , render = \_ _ -> Dict.fromList [("article", Html.text category)]
  , actions = [Succ, Append category, Append "article"]
  }
