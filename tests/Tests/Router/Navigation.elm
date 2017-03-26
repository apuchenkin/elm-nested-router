module Tests.Router.Navigation exposing (..)

import Expect
import Test exposing (..)

import Dict
import URL.Route as Route
import URL.Matcher as Matcher
import Router.Navigation as Navigation
import Tests.Mock.Router exposing (routerConfig)
import Tests.Mock.RouteConfig exposing (routeConfig)
import Tests.Mock.Routes exposing (..)

testSuite : Test
testSuite = describe "Navigation" [
    testBuildUrl
  ]

config : Route.GetConfig Route
config = .route << routeConfig

testBuildUrl : Test
testBuildUrl = let
    buildUrl = Navigation.buildUrl routerConfig
    buildURL_ = Matcher.buildURL config
  in describe "buildUrl" [
    test "home"
      <| \_ -> Expect.equal (buildURL_ (Route.route (Category "bear") Dict.empty))
      <| buildUrl (Route.route (Category "bear") Dict.empty)
    ]
