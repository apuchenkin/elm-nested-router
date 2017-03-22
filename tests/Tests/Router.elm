module Tests.Router exposing (..)

import Tests.URL.Arguments
import Tests.URL.Segments
import Tests.URL.Utils
import Tests.URL.Matcher
import Tests.Router.Helpers
import Tests.Router.Navigation
import Tests.Router.Functions

import Test exposing (Test, describe)

testSuite : Test
testSuite = describe "Router" [
    Tests.URL.Arguments.testSuite
  , Tests.URL.Segments.testSuite
  , Tests.URL.Utils.testSuite
  , Tests.URL.Matcher.testSuite
  , Tests.Router.Helpers.testSuite
  , Tests.Router.Navigation.testSuite
  , Tests.Router.Functions.testSuite
  ]
