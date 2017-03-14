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
    parent = Just (Category "test")
  , segment = Segments.static "article" </> Segments.enum "animal" ["lion", "penguin"]
  }

routes : List Route
routes = [Home, Category "bear", Category "tiger", Category "test", Post, Article]

testSuite : Test
testSuite = describe "Mather" [
    testMatch,
    testNoMatch
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
      <| \_ -> Expect.equal (Just {route = Category "test", arguments = Dict.fromList [("category", "category-name")]})
      <| Matcher.match routeConfig routes "/category/category-name"
  , test "match category"
      <| \_ -> Expect.equal (Just {route = Category "test", arguments = Dict.fromList [("category", "lion")]})
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

testNoMatch : Test
testNoMatch = describe "match" [
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

-- testBuildUrl : Test
-- testBuildUrl = describe "buildUrl"
--   [
--     test "home"
--       <| \_ -> Expect.equal "/"
--       <| buildUrl (.segment << config) (.parent << config) (Home, Dict.empty)
--   , test "category"
--       <| \_ -> Expect.equal "/param"
--       <| buildUrl (.segment << config) (.parent << config) (Page, (Dict.fromList [("category","param")]))
--   , test "subcategory"
--       <| \_ -> Expect.equal "/param/param2"
--       <| buildUrl (.segment << config) (.parent << config) (Page, (Dict.fromList [("category","param"),("subcategory","param2")]))
--   , test "subcategory"
--       <| \_ -> Expect.equal "/param/param2"
--       <| buildUrl (.segment << config) (.parent << config) (Page, (Dict.fromList [("subcategory","param2"),("category","param")]))
--   , test "item"
--       <| \_ -> Expect.equal "/param/item/123"
--       <| buildUrl (.segment << config) (.parent << config) (Subpage, (Dict.fromList [("category","param"),("item","123")]))
--   ]

-- testReversible : Test
-- testReversible = describe "reversible"
--   [
--     test "match"
--       <| \_ -> Expect.equal (Just "/")
--       <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/")
--   , test "fail by constraint"
--       <| \_ -> Expect.equal Nothing
--       <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/param")
--   , test "equal with constraint"
--       <| \_ -> Expect.equal (Just "/A")
--       <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/A")
--   , test "match 404"
--       <| \_ -> Expect.equal (Just "/404")
--       <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/404")
--   , test "match with optional param"
--       <| \_ -> Expect.equal (Just "/A/subcategory")
--       <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/A/subcategory")
--   , test "match Subpage without optional param"
--       <| \_ -> Expect.equal (Just "/B/item/3")
--       <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/B/item/3")
--   , test "match Subpage with optional param"
--       <| \_ -> Expect.equal (Just "/C/subcategory/item/4")
--       <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/C/subcategory/item/4")
--   ]
