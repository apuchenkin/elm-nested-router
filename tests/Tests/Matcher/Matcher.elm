module Tests.Matcher.Matcher exposing (..)

import Expect

-- import Dict exposing (Dict)
import Test exposing (..)
import Dict
import Matcher.Matcher as Matcher

-- type R = Home | Category String | Post
--
-- homeR : Matcher.RouteConfig R state msg
-- homeR = {
--     route = Home
--   , parent = Nothing
--   , segment = Matcher.end
--   , render = \s v -> v
--   }
--
-- routes : List (Matcher.RouteConfig R state msg)
-- routes = [homeR]

testSuite : Test
testSuite = describe "Mather" [
    -- testToString
  ]

-- testToString : Test
-- testToString = describe "toString" [
--   test "term"
--     <| \_ -> Expect.equal ""
--     <| Matcher.toString Dict.empty Matcher.Terminator
--   ]


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
