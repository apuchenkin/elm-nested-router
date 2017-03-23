module Tests.URL.Route exposing (..)

import Expect

-- import Dict exposing (Dict)
import Test exposing (..)
import Dict
import URL.Route as Route
import Tests.Mock.RouteConfig exposing (routeConfig)
import Tests.Mock.Routes exposing (..)

config : Route.GetConfig Route
config = .route << routeConfig

testSuite : Test
testSuite = describe "Mather" [
    testParents,
    testTraverse
  ]

testParents : Test
testParents = let
    parent = Route.parents config routes
  in
    describe "parents" [
      test "home"
        <| \_ -> Expect.equal []
        <| parent Home
    , test "category"
        <| \_ -> Expect.equal [Home]
        <| parent (Category "bear")
    , test "post"
        <| \_ -> Expect.equal [Home, Category "bear"]
        <| parent Post
    , test "article"
        <| \_ -> Expect.equal [Home, Category "animal"]
        <| parent (Article "animal")
    ]

testTraverse : Test
testTraverse = let
    traverse_ = Route.traverse config routes
  in describe "traverse" [
    test "Category bear"
      <| \_ -> Expect.equal [Home, Category "bear"]
      <| traverse_
      <| Category "bear"
  , test "post"
      <| \_ -> Expect.equal [Home, Category "bear", Post]
      <| traverse_ Post
  , test "article"
      <| \_ -> Expect.equal [Home, Category "animal", Article "animal"]
      <| traverse_
      <| Article "animal"
  , test "home"
      <| \_ -> Expect.equal [Home]
      <| traverse_ Home
  ]
