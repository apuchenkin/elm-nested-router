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
      <| Segments.parse Segments.end ""
  , test "parse static"
      <| \_ -> Expect.true "fail test" <| isOk
      <| Segments.parse (Segments.static "test") "test"
  , test "parse int"
      <| \_ -> Expect.true "fail parse 1" <| isOk
      <| Segments.parse (Segments.int "param1") "1"
  , test "parse string"
      <| \_ -> Expect.true "fail parse string" <| isOk
      <| Segments.parse (Segments.string "param1") "string"
  , test "parse enum"
      <| \_ -> Expect.true "fail parse enum" <| isOk
      <| Segments.parse (Segments.enum "param1" ["tiger", "bear"]) "bear"
  , test "parse regex"
      <| \_ -> Expect.true "fail parse regex" <| isOk
      <| Segments.parse (Segments.regex "param1" "^\\d\\d-\\w{3}$") "02-abc"
  , test "parse sequence"
      <| \_ -> Expect.true "fail parse sequence" <| isOk
      <| Segments.parse (Segments.static "category" </> Segments.string "param1") "category/bear"
  , test "parse sequence end"
      <| \_ -> Expect.true "fail parse sequence" <| isOk
      <| Segments.parse (Segments.static "category" </> Segments.string "param1" </> Segments.end) "category/bear"
  , test "parse sequence end 2"
      <| \_ -> Expect.true "fail parse sequence" <| isOk
      <| Segments.parse (Segments.static "category" </> Segments.string "param1" </> Segments.end) "category/bear/"
  , test "parse sequence 2"
      <| \_ -> Expect.true "fail parse sequence 2" <| isOk
      <| Segments.parse (Segments.static  "category" </> Segments.string "param1" </> Segments.static "post" </> Segments.int "param 2")
      "category/bear/post/1"
  ]

testToString : Test
testToString = describe "toString" [
    test "toString end"
      <| \_ -> Expect.equal ""
      <| Segments.toString Dict.empty Segments.end
  , test "toString static"
      <| \_ -> Expect.equal "test"
      <| Segments.toString Dict.empty
      <| Segments.static "test"
  , test "toString int"
      <| \_ -> Expect.equal "1"
      <| Segments.toString (Dict.fromList [("test", "1")])
      <| Segments.int "test"
  , test "toString string"
      <| \_ -> Expect.equal "test"
      <| Segments.toString (Dict.fromList [("test", "test")])
      <| Segments.string "test"
  , test "toString enum"
      <| \_ -> Expect.equal "lion"
      <| Segments.toString (Dict.fromList [("animal", "lion")])
      <| Segments.enum "animal" ["tiger", "lion"]
  , test "toString sequence"
      <| \_ -> Expect.equal "animal"
      <| Segments.toString Dict.empty
      <| Segments.static "animal" </> Segments.end
  , test "toString sequence 2"
      <| \_ -> Expect.equal "animal/bear"
      <| Segments.toString Dict.empty
      <| Segments.static "animal" </> Segments.static "bear" </> Segments.end
  , test "toString sequence 3"
      <| \_ -> Expect.equal "animal/lion"
      <| Segments.toString (Dict.fromList [("animal", "lion")])
      <| Segments.static "animal" </> Segments.string "animal"
  , test "toString sequence 3"
      <| \_ -> Expect.equal "animal/tiger/post/12"
      <| Segments.toString (Dict.fromList [("animal", "tiger"), ("postId", "12")])
      <| Segments.static "animal" </> Segments.string "animal" </> Segments.static "post" </> Segments.int "postId"
  ]
