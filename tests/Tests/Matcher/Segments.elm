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
    test "end"
      <| \_ -> Expect.equal (Ok "")
      <| Segments.toString Dict.empty Segments.end
  , test "static"
      <| \_ -> Expect.equal (Ok "test")
      <| Segments.toString Dict.empty
      <| Segments.static "test"
  , test "int"
      <| \_ -> Expect.equal (Ok "1")
      <| Segments.toString (Dict.fromList [("test", "1")])
      <| Segments.int "test"
  , test "string"
      <| \_ -> Expect.equal (Ok "test")
      <| Segments.toString (Dict.fromList [("test", "test")])
      <| Segments.string "test"
  , test "enum"
      <| \_ -> Expect.equal (Ok "lion")
      <| Segments.toString (Dict.fromList [("animal", "lion")])
      <| Segments.enum "animal" ["tiger", "lion"]
  , test "sequence"
      <| \_ -> Expect.equal (Ok "animal")
      <| Segments.toString Dict.empty
      <| Segments.static "animal" </> Segments.end
  , test "sequence 2"
      <| \_ -> Expect.equal (Ok "animal/bear")
      <| Segments.toString Dict.empty
      <| Segments.static "animal" </> Segments.static "bear" </> Segments.end
  , test "sequence 3"
      <| \_ -> Expect.equal (Ok "animal/lion")
      <| Segments.toString (Dict.fromList [("animal", "lion")])
      <| Segments.static "animal" </> Segments.string "animal"
  , test "sequence 3"
      <| \_ -> Expect.equal (Ok "animal/tiger/post/12")
      <| Segments.toString (Dict.fromList [("animal", "tiger"), ("postId", "12")])
      <| Segments.static "animal" </> Segments.string "animal" </> Segments.static "post" </> Segments.int "postId"
  , test "optional"
      <| \_ -> Expect.equal (Ok "animal/tiger/post/12")
      <| Segments.toString (Dict.fromList [("animal", "tiger"), ("postId", "12")])
      <| Segments.static "animal" </> Segments.string "animal" </> Segments.maybe (Segments.static "post" </> Segments.int "postId")
  , test "optional 2"
      <| \_ -> Expect.equal (Ok "animal/tiger")
      <| Segments.toString (Dict.fromList [("animal", "tiger")])
      <| Segments.static "animal" </> Segments.string "animal" </> Segments.maybe (Segments.static "post" </> Segments.int "postId")
  ]
