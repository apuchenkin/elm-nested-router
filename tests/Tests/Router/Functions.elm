module Tests.Router.Functions exposing (..)

import Dict
import Html
import Test exposing (..)
import Expect

import Router.Types     exposing (..)
import Router.Functions exposing (..)
import Router.Matcher   as Matcher

import Tests.Mock.Data exposing (..)

testSuite : Test
testSuite = describe "Functions" [
    testRunAction
  , testRender
  , testTransition
  ]

testRunAction : Test
testRunAction = describe "runAction"
  [
    test "noAction"
      <| \_ -> Expect.equal init
      <| let (r,_) = runAction noAction init in r
  , test "succ"
      <| \_ -> Expect.equal 1
      <| let (r,_) = runAction succ init in r.sum
  , test "succ"
      <| \_ -> Expect.equal "foo"
      <| let (r,_) = runAction (append "foo") init in r.str
  ]

-- render : Router route (WithRouter route state) -> Html -> (WithRouter route state) ->  Html
testRender : Test
testRender =
  let
    render_ = render router (List.map ((\h -> h router) << .handler << config) << Matcher.getPath (.parent << config))
    state_ route = let rs = init.router in {init | router = {rs | route = Just route}}
  in describe "render"
  [
    test "fail render"
      <| \_ -> Expect.equal (toString <| Html.text "error")
      <| toString <| render_ init
  , test "render home"
      <| \_ -> Expect.equal (toString <| Html.text "handlerA")
      <| toString <| render_ (state_ Home)
  , test "render Page"
      <| \_ -> Expect.equal (toString <| Html.text "0")
      <| toString <| render_ (state_ Page)
  , test "render Subpage"
      <| \_ -> Expect.equal (toString <| Html.text "")
      <| toString <| render_ (state_ Subpage)
  ]

testTransition : Test
testTransition =
  let
   state_ route = let rs = init.router in {init | router = {rs | route = Just route}}
   transition_ = transition router matcher getHandlers
  in describe "setRoute"
  [
    test "route setted"
      <| \_ -> Expect.equal (Just Home)
      <| let (Response (result,_)) = transition_ (Just (Home, Dict.empty)) init in result.router.route
  , test "route setted"
      <| \_ -> Expect.equal (Just Page)
      <| let (Response (result,_)) = transition_ (Just (Page, Dict.empty)) (state_ NotFound) in result.router.route
  , test "params setted"
      <| \_ -> Expect.equal (Dict.fromList [("param1", "value1")])
      <| let (Response (result,_)) = transition_ (Just (Subpage, Dict.fromList [("param1", "value1")])) init in result.router.params
  , test "route actions"
      <| \_ -> Expect.equal 1
      <| let (Response (result,_)) = transition_ (Just (Page, Dict.empty)) init in result.sum
  , test "route actions"
      <| \_ -> Expect.equal (2,"foo")
      <| let (Response (result,_)) = transition_ (Just (Subpage, Dict.empty)) init in (result.sum, result.str)
  , test "route actions"
      <| \_ -> Expect.equal (1,"foo")
      <| let (Response (result,_)) = transition_ (Just (Subpage, Dict.empty)) (state_ Page) in (result.sum, result.str)
  , test "route actions"
      <| \_ -> Expect.equal (0,"")
      <| let (Response (result,_)) = transition_ (Just (Subpage, Dict.empty)) (state_ Subpage) in (result.sum, result.str)
  , test "notFound"
      <| \_ -> Expect.equal Nothing
      <| let (Response (result,_)) = transition_ Nothing (state_ Subpage) in result.router.route
  ]
