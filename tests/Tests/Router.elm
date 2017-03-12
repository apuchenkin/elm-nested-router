module Tests.Router exposing (..)

import Tests.Matcher.Matcher
import Tests.Matcher.Arguments
import Tests.Matcher.Segments
-- import Tests.Router.Functions
-- import Tests.Router.Helpers

import Test exposing (Test, describe)

testSuite : Test
testSuite = describe "Router" [
    Tests.Matcher.Arguments.testSuite
  , Tests.Matcher.Segments.testSuite
  , Tests.Matcher.Matcher.testSuite
  ]
