module Tests.Matcher.Matcher exposing (..)

import Expect

-- import Dict exposing (Dict)
import Test exposing (..)
import Dict
import Matcher.Matcher as Matcher
import Matcher.Segments as Segments exposing ((</>))

type Route = Home | Category String | Post | Article

routeConfig : Matcher.GetConfig Route
routeConfig route = case route of
  Home -> {
    parent = Nothing
  , segment = Segments.end
  }
  Category "bear" -> {
    parent = Just Home
  , segment = Segments.static "category" </> Segments.static "bear"
  }
  Category "tiger" -> {
    parent = Just Home
  , segment = Segments.static "category" </> Segments.static "tiger"
  }
  Category category -> {
    parent = Just Home
  , segment = Segments.static "category" </> Segments.string "category"
  }
  Post -> {
    parent = Just (Category "bear")
  , segment = Segments.static "post" </> Segments.int "post"
  }
  Article -> {
    parent = Just (Category "animal")
  , segment = Segments.static "article" </> Segments.enum "animal" ["lion", "penguin"]
  }

routes : List Route
routes = [Home, Category "bear", Category "tiger", Category "animal", Post, Article]

testSuite : Test
testSuite = describe "Mather" [
    testMatch
  , testMatchFail
  , testBuildUrl
  , testReversible
  ]

testMatch : Test
testMatch = describe "match" [
    test "match home"
      <| \_ -> Expect.equal (Just {route = Home, arguments = Dict.empty})
      <| Matcher.match routeConfig routes ""
  , test "match home"
      <| \_ -> Expect.equal (Just {route = Home, arguments = Dict.empty})
      <| Matcher.match routeConfig routes "/"
  , test "match category bear"
      <| \_ -> Expect.equal (Just {route = Category "bear", arguments = Dict.empty})
      <| Matcher.match routeConfig routes "/category/bear"
  , test "match category bear"
      <| \_ -> Expect.equal (Just {route = Category "bear", arguments = Dict.empty})
      <| Matcher.match routeConfig routes "/category/bear/"
  , test "match category tiger"
      <| \_ -> Expect.equal (Just {route = Category "tiger", arguments = Dict.empty})
      <| Matcher.match routeConfig routes "/category/tiger"
  , test "match category tiger"
      <| \_ -> Expect.equal (Just {route = Category "tiger", arguments = Dict.empty})
      <| Matcher.match routeConfig routes "/category/tiger/"
  , test "match category"
      <| \_ -> Expect.equal (Just {route = Category "animal", arguments = Dict.fromList [("category", "category-name")]})
      <| Matcher.match routeConfig routes "/category/category-name"
  , test "match category"
      <| \_ -> Expect.equal (Just {route = Category "animal", arguments = Dict.fromList [("category", "lion")]})
      <| Matcher.match routeConfig routes "/category/lion/"
  , test "match post"
      <| \_ -> Expect.equal (Just {route = Post, arguments = Dict.fromList [("post", "42")]})
      <| Matcher.match routeConfig routes "/category/bear/post/42"
  , test "match post"
      <| \_ -> Expect.equal (Just {route = Post, arguments = Dict.fromList [("post", "12")]})
      <| Matcher.match routeConfig routes "/category/bear/post/12/"
  , test "match article"
      <| \_ -> Expect.equal (Just {route = Article, arguments = Dict.fromList [("category", "animals"), ("animal", "penguin")]})
      <| Matcher.match routeConfig routes "/category/animals/article/penguin"
  , test "match article"
      <| \_ -> Expect.equal (Just {route = Article, arguments = Dict.fromList [("category", "bear"), ("animal", "lion")]})
      <| Matcher.match routeConfig routes "/category/bear/article/lion/"
  ]

testMatchFail : Test
testMatchFail = describe "match" [
    test "match home"
      <| \_ -> Expect.equal Nothing
      <| Matcher.match routeConfig routes "/some"
  , test "match category"
      <| \_ -> Expect.equal Nothing
      <| Matcher.match routeConfig routes "/category"
  , test "match category 2"
      <| \_ -> Expect.equal Nothing
      <| Matcher.match routeConfig routes "/category/"
  , test "match post"
      <| \_ -> Expect.equal Nothing
      <| Matcher.match routeConfig routes "/category/bear/post/string"
  , test "match article"
      <| \_ -> Expect.equal Nothing
      <| Matcher.match routeConfig routes "/category/animals/article/bear"
  ]

testBuildUrl : Test
testBuildUrl = describe "buildUrl"
  [
    test "buildURL home"
      <| \_ -> Expect.equal "/"
      <| Matcher.buildURL routeConfig <| Matcher.route Home Dict.empty
  , test "buildURL category bear"
      <| \_ -> Expect.equal "/category/bear"
      <| Matcher.buildURL routeConfig <| Matcher.route (Category "bear") Dict.empty
  , test "buildURL category tiger"
      <| \_ -> Expect.equal "/category/tiger"
      <| Matcher.buildURL routeConfig <| Matcher.route (Category "tiger") Dict.empty
  , test "buildURL category animal"
      <| \_ -> Expect.equal "/category/lion"
      <| Matcher.buildURL routeConfig <| Matcher.route (Category "animal") (Dict.fromList [("category", "lion")])
  , test "buildURL post"
      <| \_ -> Expect.equal "/category/bear/post/1"
      <| Matcher.buildURL routeConfig <| Matcher.route Post (Dict.fromList [("post", "1")])
  , test "buildURL article"
      <| \_ -> Expect.equal "/category/animal/article/penguin"
      <| Matcher.buildURL routeConfig <| Matcher.route Article (Dict.fromList [("category", "animal"), ("animal", "penguin")])
  ]

testReversible : Test
testReversible = let
    home = Matcher.route Home Dict.empty
    bear = Matcher.route (Category "bear") Dict.empty
    tiger = Matcher.route (Category "tiger") Dict.empty
    animal = Matcher.route (Category "animal") (Dict.fromList [("category", "lion")])
    post = Matcher.route Post (Dict.fromList [("post", "1")])
    article = Matcher.route Article (Dict.fromList [("category", "animal"), ("animal", "penguin")])
  in describe "reversible"
  [
    test "home"
      <| \_ -> Expect.equal (Just home)
      <| Matcher.match routeConfig routes
      <| Matcher.buildURL routeConfig home
  , test "bear"
      <| \_ -> Expect.equal (Just bear)
      <| Matcher.match routeConfig routes
      <| Matcher.buildURL routeConfig bear
  , test "tiger"
      <| \_ -> Expect.equal (Just tiger)
      <| Matcher.match routeConfig routes
      <| Matcher.buildURL routeConfig tiger
  , test "animal"
      <| \_ -> Expect.equal (Just animal)
      <| Matcher.match routeConfig routes
      <| Matcher.buildURL routeConfig animal
  , test "post"
      <| \_ -> Expect.equal (Just post)
      <| Matcher.match routeConfig routes
      <| Matcher.buildURL routeConfig post
  , test "article"
      <| \_ -> Expect.equal (Just article)
      <| Matcher.match routeConfig routes
      <| Matcher.buildURL routeConfig article
  ]
