module Tests.Router.Navigation exposing (..)

import Expect
import Test exposing (..)

import Router.Navigation exposing (..)

testSuite : Test
testSuite = describe "Navigation" [
    testToString
  ]

testToString : Test
testToString = describe "fake" [
  test "term"
    <| \_ -> Expect.pass
  ]
