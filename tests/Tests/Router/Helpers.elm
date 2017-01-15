module Tests.Router.Helpers exposing (..)

import Test exposing (..)
import Expect

import Router.Types     exposing (..)
import Router.Helpers   exposing (..)

import Tests.Mock.Data exposing (..)

testSuite : Test
testSuite = describe "Helpers" [
    testNoFx
  , testDoNothing
  , testPerformTask
  , testChainAction
  , testCombineActions
  ]
--
testNoFx : Test
testNoFx = describe "noFx"
  [
    test "1"
      <| \_ -> Expect.equal (1, Cmd.none)
      <| noFx 1
  , test "2"
      <| \_ -> Expect.equal (Nothing, Cmd.none)
      <| noFx Nothing
  ]

testDoNothing : Test
testDoNothing = describe "doNothing"
  [
    test "1"
      <| \_ -> Expect.equal (Response <| (1, Cmd.none))
      <| doNothing 1
  , test "noFx"
      <| \_ -> Expect.equal (Response <| noFx Nothing)
      <| doNothing Nothing
  ]

testPerformTask : Test
testPerformTask = test "covered" <| \_ -> Expect.pass

testChainAction : Test
testChainAction = describe "chainAction"
  [
    test "noAction"
      <| \_ -> Expect.equal init
      <| let (Response (result,_)) = (noAction |> chainAction noAction) init in result
  , test "one succ"
      <| \_ -> Expect.equal 1
      <| let (Response (result,_)) = (succ |> chainAction noAction) init in result.sum
  , test "two succ"
      <| \_ -> Expect.equal 2
      <| let (Response (result,_)) = (succ |> chainAction succ) init in result.sum
  , test "append order"
      <| \_ -> Expect.equal "AB"
      <| let (Response (result,_)) = (append "A" |> chainAction (append "B")) init in result.str
  , test "combined"
      <| \_ -> Expect.equal "BA"
      <| let (Response (result,_)) = (append "B" |> chainAction (append "A")) init in result.str
  , test "combineActions"
      <| \_ -> Expect.equal (let (Response (result,_)) = (combineActions [succ, succ]) init in result)
      <| let (Response (result,_)) = (succ |> chainAction succ) init in result
  ]

testCombineActions : Test
testCombineActions = describe "combineActions"
  [
    test "noAction"
      <| \_ -> Expect.equal init
      <| let (Response (result,_)) = (combineActions [noAction, noAction, noAction]) init in result
  , test "one succ"
      <| \_ -> Expect.equal 1
      <| let (Response (result,_)) = (combineActions [succ, noAction]) init in result.sum
  , test "two succ"
      <| \_ -> Expect.equal 2
      <| let (Response (result,_)) = (combineActions [succ, succ]) init in result.sum
  , test "append order"
      <| \_ -> Expect.equal "ABC"
      <| let (Response (result,_)) = (combineActions [append "A", append "B", append "C"]) init in result.str
  , test "combined"
      <| \_ -> Expect.true "expected true"
      <| let (Response (result,_)) = (combineActions [succ, append "A", succ, append "B"]) init
      in result.str == "AB" && result.sum == 2
  ]
