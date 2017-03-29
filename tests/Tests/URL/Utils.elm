module Tests.URL.Utils exposing (..)

import Expect
import Dict
import Test exposing (..)

import URL.Route as Route
import URL.Utils exposing (..)
import Tests.Mock.RouteConfig exposing (..)
import Tests.Mock.Routes exposing (..)

config : Route.GetConfig Route
config = .route << routeConfig

testSuite : Test
testSuite = describe "Arguments" [
    testMapArguments
  , testRouteDiff
  ]


testMapArguments : Test
testMapArguments =
  let
    arguments = Dict.fromList [("category","param"), ("subcategory","param2"), ("post","4"), ("animal", "lion")]
    mapArguments_ = flip (mapArguments config) arguments
  in describe "mapArguments" [
    test "mapArguments"
      <| \_ -> Expect.equal [
        Route.route Home Dict.empty,
        Route.route (Category "bear") Dict.empty,
        Route.route Post <| Dict.fromList [("post", "4")]
      ]
      <| mapArguments_ [Home, Category "bear", Post]
  , test "mapArguments"
      <| \_ -> Expect.equal [
        Route.route Home Dict.empty,
        Route.route (Category "animal")<| Dict.fromList [("category", "param"), ("subcategory", "param2")]
      ]
      <| mapArguments_ [Home, Category "animal"]
  , test "mapArguments"
      <| \_ -> Expect.equal [
        Route.route Home Dict.empty,
        Route.route (Category "animal") <| Dict.fromList [("category", "param"), ("subcategory", "param2")],
        Route.route (Article "animal") <| Dict.fromList [("animal", "lion")]
      ]
      <| mapArguments_ [Home, Category "animal", Article "animal"]
  ]

testRouteDiff : Test
testRouteDiff =
  let
    diff = routeDiff config routes
  in describe "routeDiff"
  [
    test "Home"
      <| \_ -> Expect.equal [Home]
      <| diff Nothing
      <| Route.route Home Dict.empty
  , test "category bear"
      <| \_ -> Expect.equal [Home, Category "bear"]
      <| diff Nothing
      <| Route.route (Category "bear") Dict.empty
  , test "category bear"
      <| \_ -> Expect.equal [Category "bear"]
      <| diff (Just <| Route.route Home Dict.empty)
      <| Route.route (Category "bear") Dict.empty
  , test "post"
      <| \_ -> Expect.equal [Home, Category "bear", Post]
      <| diff Nothing
      <| Route.route Post Dict.empty
  , test "post"
      <| \_ -> Expect.equal []
      <| diff (Just <| Route.route Post Dict.empty)
      <| Route.route Post Dict.empty
  , test "post"
      <| \_ -> Expect.equal [Post]
      <| diff (Just <| Route.route Post <| Dict.fromList [("post", "4")])
      <| Route.route Post <| Dict.fromList [("post", "2")]
  , test "article"
      <| \_ -> Expect.equal [Category "animal", Article "animal"]
      <| diff (Just <| Route.route Post <| Dict.fromList [("post", "4")])
      <| Route.route (Article "animal") <| Dict.fromList [("post", "4")]
  , test "article"
      <| \_ -> Expect.equal []
      <| diff (Just <| Route.route (Article "animal") <| Dict.fromList [("animal", "lion")])
      <| Route.route (Article "animal") <| Dict.fromList [("animal", "lion")]
  , test "article"
      <| \_ -> Expect.equal [Category "animal", Article "animal"]
      <| diff (Just <| Route.route (Article "animal") <| Dict.fromList [("category", "foo"), ("animal", "lion")])
      <| Route.route (Article "animal") <| Dict.fromList [("category", "bar"), ("animal", "lion")]
  ]
