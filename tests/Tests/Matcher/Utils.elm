module Tests.Matcher.Utils exposing (..)

import Expect
import Dict
import Test exposing (..)
import Matcher.Matcher as Matcher
import Matcher.Utils exposing (..)
import Tests.Mock.Routes exposing (..)

testSuite : Test
testSuite = describe "Arguments" [
    testTraverse
  , testMapArguments
  , testRouteDiff
  ]

testTraverse : Test
testTraverse = describe "traverse" [
    test "Category bear"
      <| \_ -> Expect.equal [Home, Category "bear"]
      <| traverse routeConfig routes
      <| Category "bear"
  , test "post"
      <| \_ -> Expect.equal [Home, Category "bear", Post]
      <| traverse routeConfig routes Post
  , test "article"
      <| \_ -> Expect.equal [Home, Category "animal", Article "animal"]
      <| traverse routeConfig routes
      <| Article "animal"
  , test "home"
      <| \_ -> Expect.equal [Home]
      <| traverse routeConfig routes Home
  ]

testMapArguments : Test
testMapArguments =
  let arguments = Dict.fromList [("category","param"), ("subcategory","param2"), ("post","4"), ("animal", "lion")]
  in describe "mapArguments" [
    test "mapArguments"
      <| \_ -> Expect.equal [
        Matcher.route Home Dict.empty,
        Matcher.route (Category "bear") Dict.empty,
        Matcher.route Post <| Dict.fromList [("post", "4")]
      ]
      <| mapArguments routeConfig [Home, Category "bear", Post] arguments
  , test "mapArguments"
      <| \_ -> Expect.equal [
        Matcher.route Home Dict.empty,
        Matcher.route (Category "animal")<| Dict.fromList [("category", "param"), ("subcategory", "param2")]
      ]
      <| mapArguments routeConfig [Home, Category "animal"] arguments
  , test "mapArguments"
      <| \_ -> Expect.equal [
        Matcher.route Home Dict.empty,
        Matcher.route (Category "animal") <| Dict.fromList [("category", "param"), ("subcategory", "param2")],
        Matcher.route (Article "animal") <| Dict.fromList [("animal", "lion")]
      ]
      <| mapArguments routeConfig [Home, Category "animal", Article "animal"] arguments
  ]

testRouteDiff : Test
testRouteDiff =
  let
    diff = routeDiff routeConfig routes
  in describe "routeDiff"
  [
    test "Home"
      <| \_ -> Expect.equal [Home]
      <| diff Nothing
      <| Matcher.route Home Dict.empty
  , test "category bear"
      <| \_ -> Expect.equal [Home, Category "bear"]
      <| diff Nothing
      <| Matcher.route (Category "bear") Dict.empty
  , test "category bear"
      <| \_ -> Expect.equal [Category "bear"]
      <| diff (Just <| Matcher.route Home Dict.empty)
      <| Matcher.route (Category "bear") Dict.empty
  , test "post"
      <| \_ -> Expect.equal [Home, Category "bear", Post]
      <| diff Nothing
      <| Matcher.route Post Dict.empty
  , test "post"
      <| \_ -> Expect.equal []
      <| diff (Just <| Matcher.route Post Dict.empty)
      <| Matcher.route Post Dict.empty
  , test "post"
      <| \_ -> Expect.equal [Post]
      <| diff (Just <| Matcher.route Post <| Dict.fromList [("post", "4")])
      <| Matcher.route Post <| Dict.fromList [("post", "2")]
  , test "article"
      <| \_ -> Expect.equal [Category "animal", Article "animal"]
      <| diff (Just <| Matcher.route Post <| Dict.fromList [("post", "4")])
      <| Matcher.route (Article "animal") <| Dict.fromList [("post", "4")]
  , test "article"
      <| \_ -> Expect.equal []
      <| diff (Just <| Matcher.route (Article "animal") <| Dict.fromList [("animal", "lion")])
      <| Matcher.route (Article "animal") <| Dict.fromList [("animal", "lion")]
  , test "article"
      <| \_ -> Expect.equal [Category "animal", Article "animal"]
      <| diff (Just <| Matcher.route (Article "animal") <| Dict.fromList [("category", "foo"), ("animal", "lion")])
      <| Matcher.route (Article "animal") <| Dict.fromList [("category", "bar"), ("animal", "lion")]
  ]
