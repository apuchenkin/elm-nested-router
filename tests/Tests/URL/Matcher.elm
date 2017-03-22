module Tests.URL.Matcher exposing (..)

import Expect

-- import Dict exposing (Dict)
import Test exposing (..)
import Dict
import URL.Matcher as Matcher
import Tests.Mock.RouteConfig exposing (routeConfig)
import Tests.Mock.Routes exposing (..)

config : Matcher.GetConfig Route
config = .route << routeConfig

testSuite : Test
testSuite = describe "Mather" [
    testParents
  , testMatch
  , testMatchFail
  , testBuildUrl
  , testReversible
  , testRemoveTrailingSlash
  ]

testParents : Test
testParents = let
    parent = Matcher.parents config routes
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

testMatch : Test
testMatch = let
    match = Matcher.match config routes
  in describe "match" [
    test "match home"
      <| \_ -> Expect.equal (Just {route = Home, arguments = Dict.empty})
      <| match ""
  , test "match home"
      <| \_ -> Expect.equal (Just {route = Home, arguments = Dict.empty})
      <| match "/"
  , test "match category bear"
      <| \_ -> Expect.equal (Just {route = Category "bear", arguments = Dict.empty})
      <| match "/category/bear"
  , test "match category bear"
      <| \_ -> Expect.equal (Just {route = Category "bear", arguments = Dict.empty})
      <| match "/category/bear/"
  , test "match category tiger"
      <| \_ -> Expect.equal (Just {route = Category "tiger", arguments = Dict.empty})
      <| match "/category/tiger"
  , test "match category tiger"
      <| \_ -> Expect.equal (Just {route = Category "tiger", arguments = Dict.empty})
      <| match "/category/tiger/"
  , test "match category"
      <| \_ -> Expect.equal (Just {route = Category "animal", arguments = Dict.fromList [("category", "animal")]})
      <| match "/category/animal"
  , test "match category"
      <| \_ -> Expect.equal (Just {route = Category "animal", arguments = Dict.fromList [("category", "lion")]})
      <| match "/category/lion/"
  , test "match category"
      <| \_ -> Expect.equal (Just {route = Category "animal", arguments = Dict.fromList [("category", "animal"), ("subcategory", "lion")]})
      <| match "/category/animal/lion"
  , test "match post"
      <| \_ -> Expect.equal (Just {route = Post, arguments = Dict.fromList [("post", "42")]})
      <| match "/category/bear/post/42"
  , test "match post"
      <| \_ -> Expect.equal (Just {route = Post, arguments = Dict.fromList [("post", "12")]})
      <| match "/category/bear/post/12/"
  , test "match article"
      <| \_ -> Expect.equal (Just {route = Article "animal", arguments = Dict.fromList [("category", "animals"), ("animal", "penguin")]})
      <| match "/category/animals/article/penguin"
  , test "match article"
      <| \_ -> Expect.equal (Just {route = Article "animal", arguments = Dict.fromList [("category", "bear"), ("animal", "lion")]})
      <| match "/category/bear/article/lion/"
  , test "match article"
      <| \_ -> Expect.equal (Just {route = Article "animal", arguments = Dict.fromList [("category", "bear"), ("subcategory", "brown"), ("animal", "lion")]})
      <| match "/category/bear/brown/article/lion/"
  ]

testMatchFail : Test
testMatchFail = let
    match = Matcher.match config routes
  in describe "match" [
    test "match home"
      <| \_ -> Expect.equal Nothing
      <| match "/some"
  , test "match category"
      <| \_ -> Expect.equal Nothing
      <| match "/category"
  , test "match category 2"
      <| \_ -> Expect.equal Nothing
      <| match "/category/"
  , test "match post"
      <| \_ -> Expect.equal Nothing
      <| match "/category/bear/post/string"
  , test "match article"
      <| \_ -> Expect.equal Nothing
      <| match "/category/animals/article/bear"
  ]

testBuildUrl : Test
testBuildUrl = let
    buildURL = Matcher.buildURL config
  in describe "buildUrl"
  [
    test "home"
      <| \_ -> Expect.equal "/"
      <| buildURL <| Matcher.route Home Dict.empty
  , test "category bear"
      <| \_ -> Expect.equal "/category/bear"
      <| buildURL <| Matcher.route (Category "bear") Dict.empty
  , test "category tiger"
      <| \_ -> Expect.equal "/category/tiger"
      <| buildURL <| Matcher.route (Category "tiger") Dict.empty
  , test "category animal"
      <| \_ -> Expect.equal "/category/lion"
      <| buildURL <| Matcher.route (Category "animal") (Dict.fromList [("category", "lion")])
  , test "category animal subcategory"
      <| \_ -> Expect.equal "/category/lion/paws"
      <| buildURL <| Matcher.route (Category "animal") (Dict.fromList [("category", "lion"), ("subcategory", "paws")])
  , test "post"
      <| \_ -> Expect.equal "/category/bear/post/1"
      <| buildURL <| Matcher.route Post (Dict.fromList [("post", "1")])
  , test "article"
      <| \_ -> Expect.equal "/category/animal/article/penguin"
      <| buildURL <| Matcher.route (Article "animal") (Dict.fromList [("category", "animal"), ("animal", "penguin")])
  ]

testReversible : Test
testReversible = let
    home = Matcher.route Home Dict.empty
    bear = Matcher.route (Category "bear") Dict.empty
    tiger = Matcher.route (Category "tiger") Dict.empty
    animal = Matcher.route (Category "animal") (Dict.fromList [("category", "lion")])
    post = Matcher.route Post (Dict.fromList [("post", "1")])
    article = Matcher.route (Article "animal") (Dict.fromList [("category", "animal"), ("animal", "penguin")])
    match = Matcher.match config routes
    buildURL = Matcher.buildURL config
  in describe "reversible"
  [
    test "home"
      <| \_ -> Expect.equal (Just home)
      <| match
      <| buildURL home
  , test "bear"
      <| \_ -> Expect.equal (Just bear)
      <| match
      <| buildURL bear
  , test "tiger"
      <| \_ -> Expect.equal (Just tiger)
      <| match
      <| buildURL tiger
  , test "animal"
      <| \_ -> Expect.equal (Just animal)
      <| match
      <| buildURL animal
  , test "post"
      <| \_ -> Expect.equal (Just post)
      <| match
      <| buildURL post
  , test "article"
      <| \_ -> Expect.equal (Just article)
      <| match
      <| buildURL article
  ]

testRemoveTrailingSlash : Test
testRemoveTrailingSlash = describe "removeTrailingSlash" [
    test "slash is removed"
      <| \_ -> Expect.equal "/url/with/trailing/slash"
      <| Matcher.removeTrailingSlash "/url/with/trailing/slash/"
  , test "no slash"
      <| \_ -> Expect.equal "/url/without/trailing/slash"
      <| Matcher.removeTrailingSlash "/url/without/trailing/slash"
  , test "empty"
      <| \_ -> Expect.equal ""
      <| Matcher.removeTrailingSlash ""
  , test "just slash"
      <| \_ -> Expect.equal ""
      <| Matcher.removeTrailingSlash "/"
  ]
