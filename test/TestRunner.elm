module Main exposing (..)

import ElmTest  exposing (runSuiteHtml)

import Test.Router

main : Program Never
main = runSuiteHtml Test.Router.testSuite
