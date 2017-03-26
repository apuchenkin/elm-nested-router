module Tests.Router.Functions exposing (..)

import Dict
import Test exposing (..)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag)

import Router.Functions exposing (..)
import URL.Route as Route

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
      <| let (r,_) = none init in r
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
      <| \_ -> render_ init
      |> Query.fromHtml
      |> Query.has [ text "error" ]
  , test "render home"
      <| \_ -> render_ (state_ Home)
      |> Query.fromHtml
      |> Query.has [ text "home" ]
  , test "render post"
      <| \_ -> render_ (state_ Post)
      |> Query.fromHtml
      |> Query.has [ text "post" ]
  , test "render article"
      <| \_ -> render_ (state_ <| Article "animal")
      |> Query.fromHtml
      |> Query.has [ text "animal" ]
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
      <| let (result,_) = transition_ (Just <| Route.route Home Dict.empty) init in result.router.route
  , test "route setted"
      <| \_ -> Expect.equal (Just <| Category "bear")
      <| let (result,_) = transition_ (Just <| Route.route (Category "bear") Dict.empty) (state_ Home) in result.router.route
  , test "params setted"
      <| \_ -> Expect.equal (Dict.fromList [("param1", "value1")])
      <| let (result,_) = transition_ (Just <| Route.route Post (Dict.fromList [("param1", "value1")])) init in result.router.arguments
  , test "route actions"
      <| \_ -> Expect.equal 1
      <| let (result,_) = transition_ (Just <| Route.route Post Dict.empty) init in result.sum
  , test "route actions"
      <| \_ -> Expect.equal (1,"animalanimalarticle")
      <| let (result,_) = transition_ (Just <| Route.route (Article "animal") Dict.empty) init in (result.sum, result.str)
  , test "route actions"
      <| \_ -> Expect.equal (1,"animalanimalarticle")
      <| let (result,_) = transition_ (Just <| Route.route (Article "animal") Dict.empty) (state_ Post) in (result.sum, result.str)
  , test "route actions"
      <| \_ -> Expect.equal (0,"")
      <| let (result,_) = transition_ (Just <| Route.route (Article "animal") Dict.empty) (state_ <| Article "animal") in (result.sum, result.str)
  , test "notFound"
      <| \_ -> Expect.equal Nothing
      <| let (result,_) = transition_ Nothing (state_ <| Article "animal") in result.router.route
  ]
