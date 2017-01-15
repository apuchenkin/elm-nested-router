module Tests.Router exposing (..)

import Tests.Router.Matcher
import Tests.Router.Functions
import Tests.Router.Helpers

import Test exposing (Test, describe)

testSuite : Test
testSuite = describe "Router" [
    Tests.Router.Matcher.testSuite
  , Tests.Router.Functions.testSuite
  , Tests.Router.Helpers.testSuite
  ]
