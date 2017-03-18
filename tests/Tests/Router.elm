module Tests.Router exposing (..)

import Tests.Matcher.Arguments
import Tests.Matcher.Segments
import Tests.Matcher.Utils
import Tests.Matcher.Matcher
-- import Tests.Router.Functions
import Tests.Router.Helpers
import Tests.Router.Navigation

import Test exposing (Test, describe)

testSuite : Test
testSuite = describe "Router" [
    Tests.Matcher.Arguments.testSuite
  , Tests.Matcher.Segments.testSuite
  , Tests.Matcher.Utils.testSuite
  , Tests.Matcher.Matcher.testSuite
  , Tests.Router.Helpers.testSuite
  , Tests.Router.Navigation.testSuite
  ]
