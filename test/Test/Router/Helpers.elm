module Test.Router.Helpers exposing (..)

import ElmTest exposing (..)

import Router.Types     exposing (..)
import Router.Helpers   exposing (..)

import Test.Mock.Data exposing (..)

testSuite : Test
testSuite = suite "Helpers" [
    testNoFx
  , testDoNothing
  , testPerformTask
  , testChainAction
  , testCombineActions
  ]
--
testNoFx : Test
testNoFx = suite "noFx"
  [
    test "1"
      <| assertEqual (1, Cmd.none)
      <| noFx 1
  , test "2"
      <| assertEqual (Nothing, Cmd.none)
      <| noFx Nothing
  ]

testDoNothing : Test
testDoNothing = suite "doNothing"
  [
    test "1"
      <| assertEqual (Response <| (1, Cmd.none))
      <| doNothing 1
  , test "noFx"
      <| assertEqual (Response <| noFx Nothing)
      <| doNothing Nothing
  ]

testPerformTask : Test
testPerformTask = test "covered" pass

testChainAction : Test
testChainAction = suite "chainAction"
  [
    test "noAction"
      <| assertEqual init
      <| let (Response (result,_)) = (noAction `chainAction` noAction) init in result
  , test "one succ"
      <| assertEqual 1
      <| let (Response (result,_)) = (succ `chainAction` noAction) init in result.sum
  , test "two succ"
      <| assertEqual 2
      <| let (Response (result,_)) = (succ `chainAction` succ) init in result.sum
  , test "append order"
      <| assertEqual "AB"
      <| let (Response (result,_)) = (append "A" `chainAction` append "B") init in result.str
  , test "combined"
      <| assertEqual "BA"
      <| let (Response (result,_)) = (append "B" `chainAction` append "A") init in result.str
  , test "combineActions"
      <| assertEqual (let (Response (result,_)) = (combineActions [succ, succ]) init in result)
      <| let (Response (result,_)) = (succ `chainAction` succ) init in result
  ]

testCombineActions : Test
testCombineActions = suite "combineActions"
  [
    test "noAction"
      <| assertEqual init
      <| let (Response (result,_)) = (combineActions [noAction, noAction, noAction]) init in result
  , test "one succ"
      <| assertEqual 1
      <| let (Response (result,_)) = (combineActions [succ, noAction]) init in result.sum
  , test "two succ"
      <| assertEqual 2
      <| let (Response (result,_)) = (combineActions [succ, succ]) init in result.sum
  , test "append order"
      <| assertEqual "ABC"
      <| let (Response (result,_)) = (combineActions [append "A", append "B", append "C"]) init in result.str
  , test "combined"
      <| assert
      <| let (Response (result,_)) = (combineActions [succ, append "A", succ, append "B"]) init
      in result.str == "AB" && result.sum == 2
  ]
