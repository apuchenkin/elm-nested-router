module Test.Router where

import Test.Router.Matcher
import Test.Router.Functions

import ElmTest      exposing (Test, suite)

testSuite : Test
testSuite = suite "Router" [
    Test.Router.Matcher.testSuite,
    Test.Router.Functions.testSuite
  ]
