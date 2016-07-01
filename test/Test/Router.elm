module Test.Router exposing (..)

import Test.Router.Matcher
import Test.Router.Functions
import Test.Router.Helpers

import ElmTest      exposing (Test, suite)

testSuite : Test
testSuite = suite "Router" [
    Test.Router.Matcher.testSuite
  , Test.Router.Functions.testSuite
  , Test.Router.Helpers.testSuite
  ]
