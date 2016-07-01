module Test.Router.Functions exposing (..)

import Dict
import Html
import ElmTest exposing (..)

import Router.Types     exposing (..)
import Router.Functions exposing (..)
import Router.Matcher   as Matcher

import Test.Mock.Data exposing (..)

testSuite : Test
testSuite = suite "Functions" [
    testRunAction
  , testRender
  , testTransition
  ]

testRunAction : Test
testRunAction = suite "runAction"
  [
    test "noAction"
      <| assertEqual init
      <| let (r,_) = runAction noAction init in r
  , test "succ"
      <| assertEqual 1
      <| let (r,_) = runAction succ init in r.sum
  , test "succ"
      <| assertEqual "foo"
      <| let (r,_) = runAction (append "foo") init in r.str
  ]

-- render : Router route (WithRouter route state) -> Html -> (WithRouter route state) ->  Html
testRender : Test
testRender =
  let
    render' = render router (List.map ((\h -> h router) << .handler << config) << Matcher.getPath (.parent << config))
    state' route = let rs = init.router in {init | router = {rs | route = Just route}}
  in suite "render"
  [
    test "fail render"
      <| assertEqual (toString <| Html.text "error")
      <| toString <| render' init
  , test "render home"
      <| assertEqual (toString <| Html.text "handlerA")
      <| toString <| render' (state' Home)
  , test "render Page"
      <| assertEqual (toString <| Html.text "0")
      <| toString <| render' (state' Page)
  , test "render Subpage"
      <| assertEqual (toString <| Html.text "")
      <| toString <| render' (state' Subpage)
  ]

testTransition : Test
testTransition =
  let
   state' route = let rs = init.router in {init | router = {rs | route = Just route}}
   transition' = transition router matcher getHandlers
  in suite "setRoute"
  [
    test "route setted"
      <| assertEqual (Just Home)
      <| let (Response (result,_)) = transition' (Just (Home, Dict.empty)) init in result.router.route
  , test "route setted"
      <| assertEqual (Just Page)
      <| let (Response (result,_)) = transition' (Just (Page, Dict.empty)) (state' NotFound) in result.router.route
  , test "params setted"
      <| assertEqual (Dict.fromList [("param1", "value1")])
      <| let (Response (result,_)) = transition' (Just (Subpage, Dict.fromList [("param1", "value1")])) init in result.router.params
  , test "route actions"
      <| assertEqual 1
      <| let (Response (result,_)) = transition' (Just (Page, Dict.empty)) init in result.sum
  , test "route actions"
      <| assertEqual (2,"foo")
      <| let (Response (result,_)) = transition' (Just (Subpage, Dict.empty)) init in (result.sum, result.str)
  , test "route actions"
      <| assertEqual (1,"foo")
      <| let (Response (result,_)) = transition' (Just (Subpage, Dict.empty)) (state' Page) in (result.sum, result.str)
  , test "route actions"
      <| assertEqual (0,"")
      <| let (Response (result,_)) = transition' (Just (Subpage, Dict.empty)) (state' Subpage) in (result.sum, result.str)
  , test "notFound"
      <| assertEqual Nothing
      <| let (Response (result,_)) = transition' Nothing (state' Subpage) in result.router.route
  ]
