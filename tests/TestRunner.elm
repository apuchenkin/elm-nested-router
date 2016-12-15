module Main exposing (..)

import Legacy.ElmTest exposing (runSuite)
import Test.Router


main : Program Never
main =
    runSuite Test.Router.testSuite
