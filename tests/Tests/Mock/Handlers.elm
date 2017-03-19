module Tests.Mock.Handlers exposing (..)

import Dict
import Html exposing (Html)

import Tests.Mock.Routes as Routes exposing (..)
import Tests.Mock.Actions exposing (..)

import Router.Types    exposing (Action, WithRouter)
import Router.Types as Router

type alias WithRoute route config = { config | route: route }

routeConfig : Router.GetConfig Route State Msg
routeConfig route = case route of
    Home -> {
      route = Routes.routeConfig route
    , render = \_ _ -> Dict.fromList [("home", Html.text "home")]
    , actions = [NoOp]
    }
    Category "bear" -> {
      route = Routes.routeConfig route
    , render = \_ _ -> Dict.fromList [("category", Html.text "bear")]
    , actions = [Append ""]
    }
    Category "tiger" -> {
      route = Routes.routeConfig route
    , render = \_ _ -> Dict.fromList [("category", Html.text "tiger")]
    , actions = [NoOp, Append "tiger"]
    }
    Category category -> {
      route = Routes.routeConfig route
    , render = \_ _ -> Dict.fromList [("category", Html.text category)]
    , actions = [Append category]
    }
    Post -> {
      route = Routes.routeConfig route
    , render = \_ _ -> Dict.fromList [("post", Html.text "post")]
    , actions = [Succ]
    }
    Article category -> {
      route = Routes.routeConfig route
    , render = \_ _ -> Dict.fromList [("article", Html.text category)]
    , actions = [Succ, Append category, Append "article"]
    }
