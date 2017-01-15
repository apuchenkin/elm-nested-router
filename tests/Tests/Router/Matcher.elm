module Tests.Router.Matcher exposing (..)

import Expect

import Dict exposing (Dict)
import Test exposing (..)
import Router.Matcher exposing (..)
import Tests.Mock.Data exposing (..)

testSuite : Test
testSuite = describe "Mather" [
    testUnwrap
  , testParseUrlParams
  , testMatch
  , testBuildUrl
  , testReversible
  , testGetPath
  , testMapParams
  , testRemoveTrailingSlash
  , testRouteDiff
  ]

{-| Private -}
testUnwrap : Test
testUnwrap = describe "unwrap"
  [
    test "non-wrapped0"
      <| \_ -> Expect.equal ["/url"]
      <| unwrap "/url"
  , test "non-wrapped1"
      <| \_ -> flip Expect.equal ["static/static2"]
      <| unwrap "static/static2"
  , test "non-wrapped2"
      <| \_ -> flip Expect.equal ["static/:static2"]
      <| unwrap "static/:static2"
  , test "non-wrapped3"
      <| \_ -> flip Expect.equal [":static/static2"]
      <| unwrap ":static/static2"

  , test "wrapped1"
      <| \_ -> Expect.equal ["test", ""]
      <| unwrap "[test]"
  , test "wrapped2"
      <| \_ -> Expect.equal ["/:test", ""]
      <| unwrap "[/:test]"
  , test "wrapped3"
      <| \_ -> Expect.equal ["/path/test", "/path"]
      <| unwrap "/path[/test]"
  , test "wrapped4"
      <| \_ -> Expect.equal ["/:substring/:string", "/:string"]
      <| unwrap "[/:substring]/:string"

  , test "two-wrapped"
      <| \_ -> Expect.equal ["/:string/:substring", "/:string", ""]
      <| unwrap "[/:string[/:substring]]"
  , test "two-wrapped"
      <| \_ -> Expect.equal ["/path/:string/:substring", "/path/:string", "/path"]
      <| unwrap "/path[/:string[/:substring]]"
  , test "two-wrapped"
      <| \_ -> Expect.equal ["/path/:string/:substring/sub", "/path/:string/sub", "/path/sub"]
      <| unwrap "/path[/:string[/:substring]]/sub"
  , test "two-wrapped"
      <| \_ -> Expect.equal ["/path/:string/:substring", "/path/:substring", "/path/:string", "/path"]
      <| unwrap "/path[/:string][/:substring]"
  ]

{-| Private -}
testParseUrlParams : Test
testParseUrlParams = describe "parseUrlParams"
  [
    test "plain"
      <| \_ -> Expect.equal (Ok (Dict.empty, ""))
      <| parseUrlParams "/url" Dict.empty "/url"
  , test "empty"
      <| \_ -> Expect.equal (Ok (Dict.empty, ""))
      <| parseUrlParams "" Dict.empty ""
  , test "empty2"
      <| \_ -> Expect.equal (Ok (Dict.empty, "/url"))
      <| parseUrlParams "" Dict.empty "/url"
  , test "param"
      <| \_ -> Expect.equal (Ok ((Dict.fromList [("param","value")]), ""))
      <| parseUrlParams "/:param" Dict.empty "/value"
  , test "combined1"
      <| \_ -> Expect.equal (Ok ((Dict.fromList [("param","value")]), ""))
      <| parseUrlParams "/path/:param" Dict.empty "/path/value"
  , test "combined2"
      <| \_ -> Expect.equal (Ok ((Dict.fromList [("path","value")]), ""))
      <| parseUrlParams "/:path/param" Dict.empty "/value/param"
  , test "combined3"
      <| \_ -> Expect.equal (Ok ((Dict.fromList [("path","value1"), ("param","value2")]), ""))
      <| parseUrlParams "/:path/:param" Dict.empty "/value1/value2"
  , test "fail"
      <| \_ -> Expect.true "expected to fail"
      <| case parseUrlParams "/url" Dict.empty "/path" of
        Ok _ -> False
        Err _ -> True
  ]

testMatch : Test
testMatch = describe "match"
  [
    test "match Home"
      <| \_ -> Expect.equal (Just (Home, Dict.empty))
      <| match config routes "/"
  , test "match Page"
      <| \_ -> Expect.equal (Just (Page, (Dict.fromList [("category","B")])))
      <| match config routes "/B"
  , test "no match Page by constraint"
      <| \_ -> Expect.equal Nothing
      <| match config routes "/D"
  , test "match NotFound"
      <| \_ -> Expect.equal (Just (NotFound, Dict.empty))
      <| match config routes "/404"
  , test "match Page with optional params"
      <| \_ -> Expect.equal (Just (Page, (Dict.fromList [("category","A"),("subcategory","param2")])))
      <| match config routes "/A/param2"
  , test "match Subpage without optional params"
      <| \_ -> Expect.equal (Just (Subpage, Dict.fromList [("category","C"),("item","3")]))
      <| match config routes "/C/item/3"
  , test "no-match Page without optional params"
      <| \_ -> Expect.equal Nothing
      <| match config routes "/C/item/item3"
  , test "match Subpage"
      <| \_ -> Expect.equal (Just (Subpage, Dict.fromList [("category","A"),("subcategory","param2"),("item","4")]))
      <| match config routes "/A/param2/item/4"
  , test "no-match by pattern"
      <| \_ -> Expect.equal (Nothing)
      <| match config routes "/B/param2/param3"
  , test "no-match by pattern"
      <| \_ -> Expect.equal (Nothing)
      <| match config routes "/C/param2/item/4/4"
  ]

testBuildUrl : Test
testBuildUrl = describe "buildUrl"
  [
    test "home"
      <| \_ -> Expect.equal "/"
      <| buildUrl (.segment << config) (.parent << config) (Home, Dict.empty)
  , test "category"
      <| \_ -> Expect.equal "/param"
      <| buildUrl (.segment << config) (.parent << config) (Page, (Dict.fromList [("category","param")]))
  , test "subcategory"
      <| \_ -> Expect.equal "/param/param2"
      <| buildUrl (.segment << config) (.parent << config) (Page, (Dict.fromList [("category","param"),("subcategory","param2")]))
  , test "subcategory"
      <| \_ -> Expect.equal "/param/param2"
      <| buildUrl (.segment << config) (.parent << config) (Page, (Dict.fromList [("subcategory","param2"),("category","param")]))
  , test "item"
      <| \_ -> Expect.equal "/param/item/123"
      <| buildUrl (.segment << config) (.parent << config) (Subpage, (Dict.fromList [("category","param"),("item","123")]))
  ]

testReversible : Test
testReversible = describe "reversible"
  [
    test "match"
      <| \_ -> Expect.equal (Just "/")
      <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/")
  , test "fail by constraint"
      <| \_ -> Expect.equal Nothing
      <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/param")
  , test "equal with constraint"
      <| \_ -> Expect.equal (Just "/A")
      <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/A")
  , test "match 404"
      <| \_ -> Expect.equal (Just "/404")
      <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/404")
  , test "match with optional param"
      <| \_ -> Expect.equal (Just "/A/subcategory")
      <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/A/subcategory")
  , test "match Subpage without optional param"
      <| \_ -> Expect.equal (Just "/B/item/3")
      <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/B/item/3")
  , test "match Subpage with optional param"
      <| \_ -> Expect.equal (Just "/C/subcategory/item/4")
      <| Maybe.map (buildUrl (.segment << config) (.parent << config)) <| (match config routes "/C/subcategory/item/4")
  ]

testGetPath : Test
testGetPath = describe "getPath" [
    test "mapParams"
      <| \_ -> Expect.equal [Home, Page, Subpage]
      <| getPath (.parent << config) Subpage
  , test "mapParams"
      <| \_ -> Expect.equal [Home, Page]
      <| getPath (.parent << config) Page
  , test "mapParams"
      <| \_ -> Expect.equal [Home]
      <| getPath (.parent << config) Home
  ]

testMapParams : Test
testMapParams =
  let params = Dict.fromList [("category","param"),("subcategory","param2"),("item","4")]
  in describe "mapParams" [
    test "mapParams"
      <| \_ -> Expect.equal [(Home, Dict.empty), (Page, Dict.fromList [("category","param"),("subcategory","param2")]), (Subpage, Dict.fromList [("item","4")])]
      <| mapParams Tests.Mock.Data.matcher [Home, Page, Subpage] params
  ]

testRemoveTrailingSlash : Test
testRemoveTrailingSlash = describe "removeTrailingSlash" [
    test "slash is removed"
      <| \_ -> Expect.equal "/url/with/trailing/slash"
      <| removeTrailingSlash "/url/with/trailing/slash/"
  , test "no slash"
      <| \_ -> Expect.equal "/url/without/trailing/slash"
      <| removeTrailingSlash "/url/without/trailing/slash"
  , test "empty"
      <| \_ -> Expect.equal ""
      <| removeTrailingSlash ""
  , test "just slash"
      <| \_ -> Expect.equal ""
      <| removeTrailingSlash "/"
  ]

-- routeDiff : (route -> RouteConfig route state) -> Maybe (Route route) -> Route route -> List route
testRouteDiff : Test
testRouteDiff =
  let
    routeDiff_ = routeDiff Tests.Mock.Data.matcher
  in describe "routeDiff"
  [
    test "length"
      <| \_ -> Expect.equal 1
      <| List.length <| routeDiff_ Nothing (Home, Dict.empty)
  , test "length"
      <| \_ -> Expect.equal 3
      <| List.length <| routeDiff_ Nothing (Subpage, Dict.empty)
  , test "no transition - no handlers"
      <| \_ -> Expect.equal 0
      <| List.length <| routeDiff_ (Just (Home, Dict.empty)) (Home, Dict.empty)
  , test "unmatched params has no effects"
      <| \_ -> Expect.equal 0
      <| List.length <| routeDiff_ (Just (Home, Dict.empty)) (Home, Dict.fromList [("param1", "value1")])
  , test "matched params does matter"
      <| \_ -> Expect.equal 1
      <| List.length <| routeDiff_ (Just (Page, Dict.fromList [("category", "bar")])) (Page, Dict.fromList [("category", "foo")])
  , test "matched params does matter"
      <| \_ -> Expect.equal 2
      <| List.length <| routeDiff_ (Just (Subpage, Dict.fromList [("category", "bar")])) (Subpage, Dict.fromList [("category", "foo")])
  ]
