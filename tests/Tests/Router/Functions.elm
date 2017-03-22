module Tests.Router.Functions exposing (..)

import Dict
import Html
import Test exposing (..)
import Expect

import Router.Functions exposing (..)
import URL.Matcher as Matcher

import Tests.Mock.Actions exposing (..)
import Tests.Mock.Routes exposing (..)
import Tests.Mock.Router exposing (router)

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
      <| let (r,_) = noAction init in r
  , test "succ"
      <| \_ -> Expect.equal 1
      <| let (r,_) = succ init in r.sum
  , test "append"
      <| \_ -> Expect.equal "foo"
      <| let (r,_) = (append "foo") init in r.str
  ]

testRender : Test
testRender =
  let
    render_ = render router
    state_ route = let rs = init.router in {init | router = {rs | route = Just route}}
  in describe "render" [
    test "fail render"
      <| \_ -> Expect.equal (toString <| Html.text "error")
      <| toString <| render_ init
  , test "render home"
      <| \_ -> Expect.equal (toString <| Html.text "home")
      <| toString <| render_ (state_ Home)
  , test "render post"
      <| \_ -> Expect.equal (toString <| Html.text "post")
      <| toString <| render_ (state_ Post)
  , test "render article"
      <| \_ -> Expect.equal (toString <| Html.text "animal")
      <| toString <| render_ (state_ <| Article "animal")
  ]

testTransition : Test
testTransition =
  let
   state_ route = let rs = init.router in {init | router = {rs | route = Just route}}
   transition_ = transition router
  in describe "transition"
  [
    test "route setted"
      <| \_ -> Expect.equal (Just Home)
      <| let (result,_) = transition_ (Just <| Matcher.route Home Dict.empty) init in result.router.route
  , test "route setted"
      <| \_ -> Expect.equal (Just <| Category "bear")
      <| let (result,_) = transition_ (Just <| Matcher.route (Category "bear") Dict.empty) (state_ Home) in result.router.route
  , test "params setted"
      <| \_ -> Expect.equal (Dict.fromList [("param1", "value1")])
      <| let (result,_) = transition_ (Just <| Matcher.route Post (Dict.fromList [("param1", "value1")])) init in result.router.arguments
  , test "route actions"
      <| \_ -> Expect.equal 1
      <| let (result,_) = transition_ (Just <| Matcher.route Post Dict.empty) init in result.sum
  , test "route actions"
      <| \_ -> Expect.equal (1,"animalanimalarticle")
      <| let (result,_) = transition_ (Just <| Matcher.route (Article "animal") Dict.empty) init in (result.sum, result.str)
  , test "route actions"
      <| \_ -> Expect.equal (1,"animalanimalarticle")
      <| let (result,_) = transition_ (Just <| Matcher.route (Article "animal") Dict.empty) (state_ Post) in (result.sum, result.str)
  , test "route actions"
      <| \_ -> Expect.equal (0,"")
      <| let (result,_) = transition_ (Just <| Matcher.route (Article "animal") Dict.empty) (state_ <| Article "animal") in (result.sum, result.str)
  , test "notFound"
      <| \_ -> Expect.equal Nothing
      <| let (result,_) = transition_ Nothing (state_ <| Article "animal") in result.router.route
  ]
