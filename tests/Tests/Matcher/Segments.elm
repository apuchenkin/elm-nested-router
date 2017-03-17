module Tests.Matcher.Segments exposing (..)

import Expect exposing (Expectation)

-- import Dict exposing (Dict)
import Test exposing (..)
import Dict
import Matcher.Segments as Segments exposing (..)

testSuite : Test
testSuite = describe "Segments" [
    testCombine
  , testParseSuccess
  , testToString
  ]

expectOk : Result error value -> Expectation
expectOk result = case result of
  Ok _ -> Expect.pass
  Err msg -> Expect.fail <| Basics.toString msg

-- Verivies that all terminators from the beginning and middle are ignored
testCombine : Test
testCombine = describe "combine" [
    test "end"
      <| \_ -> Expect.equal end
      <| end </> end
  , test "end"
      <| \_ -> Expect.equal end
      <| end </> end </> end </> end </> end </> end
  , test "end + static"
      <| \_ -> Expect.equal (static "url" </> end)
      <| end </> end </> static "url" </> end </> end </> end
  ]
testParseSuccess : Test
testParseSuccess = describe "parse" [
    test "end"
      <| \_ -> expectOk
      <| parse "" end
  , test "static"
      <| \_ -> expectOk
      <| parse "test"
      <| static "test"
  , test "int"
      <| \_ -> expectOk
      <| parse "1"
      <| int "int-param"
  , test "string"
      <| \_ -> expectOk
      <| parse "string"
      <| string "param1"
  , test "enum"
      <| \_ -> expectOk
      <| parse "bear"
      <| enum "param1" ["tiger", "bear"]
  , test "regex"
      <| \_ -> expectOk
      <| parse "02-abc"
      <| regex "param1" "^\\d\\d-\\w{3}$"
  , test "sequence"
      <| \_ -> expectOk
      <| parse "category/bear"
      <| static "category" </> string "param1"
  , test "sequence"
      <| \_ -> expectOk
      <| parse "category/bear"
      <| end </> static "category" </> string "param1"
  , test "sequence end"
      <| \_ -> expectOk
      <| parse "category/bear"
      <| static "category" </> string "param1" </> end
  , test "sequence"
      <| \_ -> expectOk
      <| parse "category/bear"
      <| end </> static "category" </> end </> end </> end </> string "param1" </> end
  , test "sequence end 2"
      <| \_ -> expectOk
      <| parse "category/bear/"
      <| static "category" </> string "param1" </> end
  , test "sequence 2"
      <| \_ -> expectOk
      <| parse "category/bear/post/1"
      <| static "category" </> string "param1" </> static "post" </> int "param 2"
  , test "optional"
      <| \_ -> expectOk
      <| parse "category/bear/post/1"
      <| static "category" </> string "param1" </> maybe (int "param2") </> static "post" </> int "param 3"
  ]

testToString : Test
testToString = describe "toString" [
    test "end"
      <| \_ -> Expect.equal (Ok "")
      <| Segments.toString Dict.empty end
  , test "static"
      <| \_ -> Expect.equal (Ok "test")
      <| Segments.toString Dict.empty
      <| static "test"
  , test "int"
      <| \_ -> Expect.equal (Ok "1")
      <| Segments.toString (Dict.fromList [("test", "1")])
      <| int "test"
  , test "string"
      <| \_ -> Expect.equal (Ok "test")
      <| Segments.toString (Dict.fromList [("test", "test")])
      <| string "test"
  , test "enum"
      <| \_ -> Expect.equal (Ok "lion")
      <| Segments.toString (Dict.fromList [("animal", "lion")])
      <| enum "animal" ["tiger", "lion"]
  , test "sequence"
      <| \_ -> Expect.equal (Ok "animal")
      <| Segments.toString Dict.empty
      <| static "animal" </> end
  , test "sequence 2"
      <| \_ -> Expect.equal (Ok "animal/bear")
      <| Segments.toString Dict.empty
      <| static "animal" </> static "bear" </> end
  , test "sequence 3"
      <| \_ -> Expect.equal (Ok "animal/lion")
      <| Segments.toString (Dict.fromList [("animal", "lion")])
      <| static "animal" </> string "animal"
  , test "sequence 3"
      <| \_ -> Expect.equal (Ok "animal/tiger/post/12")
      <| Segments.toString (Dict.fromList [("animal", "tiger"), ("postId", "12")])
      <| static "animal" </> string "animal" </> static "post" </> int "postId"
  , test "optional"
      <| \_ -> Expect.equal (Ok "animal/tiger/post/12")
      <| Segments.toString (Dict.fromList [("animal", "tiger"), ("postId", "12")])
      <| static "animal" </> string "animal" </> maybe (static "post" </> int "postId")
  , test "optional 2"
      <| \_ -> Expect.equal (Ok "animal/tiger")
      <| Segments.toString (Dict.fromList [("animal", "tiger")])
      <| static "animal" </> string "animal" </> maybe (static "post" </> int "postId")
  ]
