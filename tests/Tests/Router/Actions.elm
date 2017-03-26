module Tests.Router.Actions exposing (..)

import Expect
import Test exposing (..)

import Router.Actions exposing (..)
import Tests.Mock.Actions exposing (..)

testSuite : Test
testSuite = describe "Helpers" [
    testNoFx
  , testPerformTask
  , testChainAction
  , testFoldActions
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

testPerformTask : Test
testPerformTask = test "covered" <| \_ -> Expect.pass

testChainAction : Test
testChainAction = describe "chainAction"
  [
    test "noAction"
      <| \_ -> Expect.equal init
      <| let (result,_) = (none |> chainAction none) init in result
  , test "one succ"
      <| \_ -> Expect.equal 1
      <| let (result,_) = (succ |> chainAction none) init in result.sum
  , test "two succ"
      <| \_ -> Expect.equal 2
      <| let (result,_) = (succ |> chainAction succ) init in result.sum
  , test "append order"
      <| \_ -> Expect.equal "AB"
      <| let (result,_) = (append "B" |> chainAction (append "A")) init in result.str
  , test "combined"
      <| \_ -> Expect.equal "BA"
      <| let (result,_) = (append "A" |> chainAction (append "B")) init in result.str
  , test "combineActions"
      <| \_ -> Expect.equal (let (result,_) = (foldActions [succ, succ]) init in result)
      <| let (result,_) = (succ |> chainAction succ) init in result
  ]

testFoldActions : Test
testFoldActions = describe "foldActions"
  [
    test "noAction"
      <| \_ -> Expect.equal init
      <| let (result,_) = (foldActions [none, none, none]) init in result
  , test "one succ"
      <| \_ -> Expect.equal 1
      <| let (result,_) = (foldActions [succ, none]) init in result.sum
  , test "two succ"
      <| \_ -> Expect.equal 2
      <| let (result,_) = (foldActions [succ, succ]) init in result.sum
  , test "append order"
      <| \_ -> Expect.equal "ABC"
      <| let (result,_) = (foldActions [append "A", append "B", append "C"]) init in result.str
  , test "combined"
      <| \_ -> Expect.true "expected true"
      <| let (result,_) = (foldActions [succ, append "A", succ, append "B"]) init
      in result.str == "AB" && result.sum == 2
  ]
