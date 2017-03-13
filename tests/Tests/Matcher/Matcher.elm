module Tests.Matcher.Matcher exposing (..)

import Expect

-- import Dict exposing (Dict)
import Test exposing (..)
import Dict
import Matcher.Matcher as Matcher
import Matcher.Segments as Segments exposing ((</>))

type Route = Home | Category String | Post

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

routes : List Route
routes = [Home, Category "bear", Category "tiger", Category "test", Post]

testSuite : Test
testSuite = describe "Mather" [
    testMatch
  ]

testMatch : Test
testMatch = describe "match" [
    test "match home"
      <| \_ -> Expect.equal (Just {route = Home, arguments = Dict.empty})
      <| Matcher.match routeConfig routes "/"
  , test "match category bear"
      <| \_ -> Expect.equal (Just {route = Category "bear", arguments = Dict.empty})
      <| Matcher.match routeConfig routes "/category/bear"
  , test "match category tiger"
      <| \_ -> Expect.equal (Just {route = Category "tiger", arguments = Dict.empty})
      <| Matcher.match routeConfig routes "/category/tiger"
  , test "match category"
      <| \_ -> Expect.equal (Just {route = Category "test", arguments = Dict.fromList [("category", "category-name")]})
      <| Matcher.match routeConfig routes "/category/category-name"
  , test "match post"
      <| \_ -> Expect.equal (Just {route = Post, arguments = Dict.fromList [("post", "42")]})
      <| Matcher.match routeConfig routes "/category/bear/post/42"
  ]

-- testMatch : Test
-- testMatch = describe "match"
--   [
--     test "match Home"
--       <| \_ -> Expect.equal (Just (Home, Dict.empty))
--       <| match config routes "/"
--   , test "match Page"
--       <| \_ -> Expect.equal (Just (Page, (Dict.fromList [("category","B")])))
--       <| match config routes "/B"
--   , test "no match Page by constraint"
--       <| \_ -> Expect.equal Nothing
--       <| match config routes "/D"
--   , test "match NotFound"
--       <| \_ -> Expect.equal (Just (NotFound, Dict.empty))
--       <| match config routes "/404"
--   , test "match Page with optional params"
--       <| \_ -> Expect.equal (Just (Page, (Dict.fromList [("category","A"),("subcategory","param2")])))
--       <| match config routes "/A/param2"
--   , test "match Subpage without optional params"
--       <| \_ -> Expect.equal (Just (Subpage, Dict.fromList [("category","C"),("item","3")]))
--       <| match config routes "/C/item/3"
--   , test "no-match Page without optional params"
--       <| \_ -> Expect.equal Nothing
--       <| match config routes "/C/item/item3"
--   , test "match Subpage"
--       <| \_ -> Expect.equal (Just (Subpage, Dict.fromList [("category","A"),("subcategory","param2"),("item","4")]))
--       <| match config routes "/A/param2/item/4"
--   , test "no-match by pattern"
--       <| \_ -> Expect.equal (Nothing)
--       <| match config routes "/B/param2/param3"
--   , test "no-match by pattern"
--       <| \_ -> Expect.equal (Nothing)
--       <| match config routes "/C/param2/item/4/4"
--   ]
--
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
