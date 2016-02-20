module Test.Router.Matcher where

import Dict         exposing (Dict)
import ElmTest      exposing (..)
import Router.Matcher   exposing (..)
import Test.Mock.Data   exposing (..)

testSuite : Test
testSuite = suite "Mather" [
    testUnwrap,
    testParseUrlParams,
    testMatch,
    testBuildUrl,
    testReversible,
    testGetPath,
    testMapParams
  ]

{-| Private -}
testUnwrap : Test
testUnwrap = suite "unwrap"
  [
    test "non-wrapped0"
      <| assertEqual ["/url"]
      <| unwrap "/url"
  , test "non-wrapped1"
      <| flip assertEqual ["static/static2"]
      <| unwrap "static/static2"
  , test "non-wrapped2"
      <| flip assertEqual ["static/:static2"]
      <| unwrap "static/:static2"
  , test "non-wrapped3"
      <| flip assertEqual [":static/static2"]
      <| unwrap ":static/static2"

  , test "wrapped1"
      <| assertEqual ["test", ""]
      <| unwrap "[test]"
  , test "wrapped2"
      <| assertEqual ["/:test", ""]
      <| unwrap "[/:test]"
  , test "wrapped3"
      <| assertEqual ["/path/test", "/path"]
      <| unwrap "/path[/test]"
  , test "wrapped4"
      <| assertEqual ["/:substring/:string", "/:string"]
      <| unwrap "[/:substring]/:string"

  , test "two-wrapped"
      <| assertEqual ["/:string/:substring", "/:string", ""]
      <| unwrap "[/:string[/:substring]]"
  , test "two-wrapped"
      <| assertEqual ["/path/:string/:substring", "/path/:string", "/path"]
      <| unwrap "/path[/:string[/:substring]]"
  , test "two-wrapped"
      <| assertEqual ["/path/:string/:substring/sub", "/path/:string/sub", "/path/sub"]
      <| unwrap "/path[/:string[/:substring]]/sub"
  , test "two-wrapped"
      <| assertEqual ["/path/:string/:substring", "/path/:substring", "/path/:string", "/path"]
      <| unwrap "/path[/:string][/:substring]"
  ]

{-| Private -}
testParseUrlParams : Test
testParseUrlParams = suite "parseUrlParams"
  [
    test "plain"
      <| assertEqual (Ok Dict.empty, "")
      <| parseUrlParams "/url" Dict.empty "/url"
  , test "param"
      <| assertEqual (Ok (Dict.fromList [("param","value")]), "")
      <| parseUrlParams "/:param" Dict.empty "/value"
  , test "combined1"
      <| assertEqual (Ok (Dict.fromList [("param","value")]), "")
      <| parseUrlParams "/path/:param" Dict.empty "/path/value"
  , test "combined2"
      <| assertEqual (Ok (Dict.fromList [("path","value")]), "")
      <| parseUrlParams "/:path/param" Dict.empty "/value/param"
  , test "combined3"
      <| assertEqual (Ok (Dict.fromList [("path","value1"), ("param","value2")]), "")
      <| parseUrlParams "/:path/:param" Dict.empty "/value1/value2"
  , test "fail"
      <| assertEqual (Err (["expected \"/url\""]),"/path")
      <| parseUrlParams "/url" Dict.empty "/path"
  ]

testMatch : Test
testMatch = suite "match"
  [
    test "match"
      <| assertEqual (Just (Home, Dict.empty))
      <| match routeMap routeTree "/"
  , test "match"
      <| assertEqual (Just (Page, (Dict.fromList [("category","param")])))
      <| match routeMap routeTree "/param"
  , test "match"
      <| assertEqual (Just (NotFound, Dict.empty))
      <| match routeMap routeTree "/404"
  , test "match"
      <| assertEqual (Just (Page, (Dict.fromList [("category","param"),("subcategory","param2")])))
      <| match routeMap routeTree "/param/param2"
  , test "match"
      <| assertEqual (Just (Subpage,Dict.fromList [("category","param"),("item","3")]))
      <| match routeMap routeTree "/param/item/3"
  , test "match"
      <| assertEqual (Just (Subpage,Dict.fromList [("category","param"),("subcategory","param2"),("item","4")]))
      <| match routeMap routeTree "/param/param2/item/4"
  , test "no-match"
      <| assertEqual (Nothing)
      <| match routeMap routeTree "/param/param2/param3"
  , test "no-match"
      <| assertEqual (Nothing)
      <| match routeMap routeTree "/param/param2/item/4/4"
  ]

testBuildUrl : Test
testBuildUrl = suite "buildUrl"
  [
    test "home"
      <| assertEqual "/"
      <| buildUrl (fst << routeMap) routeTree (Home, Dict.empty)
  , test "category"
      <| assertEqual "/param"
      <| buildUrl (fst << routeMap) routeTree (Page, (Dict.fromList [("category","param")]))
  , test "subcategory"
      <| assertEqual "/param/param2"
      <| buildUrl (fst << routeMap) routeTree (Page, (Dict.fromList [("category","param"),("subcategory","param2")]))
  , test "subcategory"
      <| assertEqual "/param/param2"
      <| buildUrl (fst << routeMap) routeTree (Page, (Dict.fromList [("subcategory","param2"),("category","param")]))
  , test "item"
      <| assertEqual "/param/item/123"
      <| buildUrl (fst << routeMap) routeTree (Subpage, (Dict.fromList [("category","param"),("item","123")]))
  ]

testReversible : Test
testReversible = suite "reversible"
  [
    test "match"
      <| assertEqual (Just "/")
      <| Maybe.map (buildUrl (fst << routeMap) routeTree) <| (match routeMap routeTree "/")
  , test "match"
      <| assertEqual (Just "/param")
      <| Maybe.map (buildUrl (fst << routeMap) routeTree) <| (match routeMap routeTree "/param")
  , test "match"
      <| assertEqual (Just "/404")
      <| Maybe.map (buildUrl (fst << routeMap) routeTree) <| (match routeMap routeTree "/404")
  , test "match"
      <| assertEqual (Just "/param/param2")
      <| Maybe.map (buildUrl (fst << routeMap) routeTree) <| (match routeMap routeTree "/param/param2")
  , test "match"
      <| assertEqual (Just "/param/item/3")
      <| Maybe.map (buildUrl (fst << routeMap) routeTree) <| (match routeMap routeTree "/param/item/3")
  , test "match"
      <| assertEqual (Just "/param/param2/item/4")
      <| Maybe.map (buildUrl (fst << routeMap) routeTree) <| (match routeMap routeTree "/param/param2/item/4")
  ]

testGetPath : Test
testGetPath = suite "getPath" [
    test "mapParams"
      <| assertEqual [Home, Page, Subpage]
      <| getPath Subpage routeTree
  , test "mapParams"
      <| assertEqual [Home, Page]
      <| getPath Page routeTree
  , test "mapParams"
      <| assertEqual [Home]
      <| getPath Home routeTree
  ]

testMapParams : Test
testMapParams =
  let params = Dict.fromList [("category","param"),("subcategory","param2"),("item","4")]
  in suite "mapParams" [
    test "mapParams"
      <| assertEqual [(Home, Dict.empty), (Page, Dict.fromList [("category","param"),("subcategory","param2")]), (Subpage, Dict.fromList [("item","4")])]
      <| mapParams (fst << routeMap) (getPath Subpage routeTree) params
  ]
