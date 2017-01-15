port module Main exposing (..)

import Tests.Router
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit Tests.Router.testSuite


port emit : ( String, Value ) -> Cmd msg
