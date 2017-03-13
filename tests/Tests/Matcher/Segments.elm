module Tests.Matcher.Segments exposing (..)

import Expect

-- import Dict exposing (Dict)
import Test exposing (..)
import Dict
import Matcher.Segments as Segments exposing ((</>))

testSuite : Test
testSuite = describe "Segments" [
    testParseSuccess,
    testToString
  ]

isOk : Result e v -> Bool
isOk result = case result of
  Ok _ -> True
  Err _ -> False

testParseSuccess : Test
testParseSuccess = describe "parse" [
    test "parse end"
      <| \_ -> Expect.true "fail \"\"" <| isOk
      <| Segments.parse "" (Segments.end)
  , test "parse static"
      <| \_ -> Expect.true "fail test" <| isOk
      <| Segments.parse "test" (Segments.static "test")
  , test "parse int"
      <| \_ -> Expect.true "fail parse 1" <| isOk
      <| Segments.parse "1" (Segments.int "param1")
  , test "parse string"
      <| \_ -> Expect.true "fail parse string" <| isOk
      <| Segments.parse "string" (Segments.string "param1")
  , test "parse enum"
      <| \_ -> Expect.true "fail parse enum" <| isOk
      <| Segments.parse "bear" (Segments.enum "param1" ["tiger", "bear"])
  , test "parse regex"
      <| \_ -> Expect.true "fail parse regex" <| isOk
      <| Segments.parse "02-abc" (Segments.regex "param1" "^\\d\\d-\\w{3}$")
  , test "parse sequence"
      <| \_ -> Expect.true "fail parse sequence" <| isOk
      <| Segments.parse "category/bear" (Segments.static "category" </> Segments.string "param1")
  , test "parse sequence end"
      <| \_ -> Expect.true "fail parse sequence" <| isOk
      <| Segments.parse "category/bear" (Segments.static "category" </> Segments.string "param1" </> Segments.end)
  , test "parse sequence end 2"
      <| \_ -> Expect.true "fail parse sequence" <| isOk
      <| Segments.parse "category/bear/" (Segments.static "category" </> Segments.string "param1" </> Segments.end)
  , test "parse sequence 2"
      <| \_ -> Expect.true "fail parse sequence 2" <| isOk
      <| Segments.parse "category/bear/post/1"
      <| Segments.static "category" </> Segments.string "param1" </> Segments.static "post" </> Segments.int "param 2"
  ]

testToString : Test
testToString = describe "toString" [
  test "term"
    <| \_ -> Expect.equal ""
    <| Segments.toString Dict.empty Segments.end
  ]
