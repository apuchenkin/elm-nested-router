module Tests.URL.Arguments exposing (..)

import Expect
import Dict
import Test exposing (..)

import URL.Arguments as Arguments exposing (..)

testSuite : Test
testSuite = describe "Arguments" [
    testGetName,
    testToString,
    testToStringFail
  ]

testGetName : Test
testGetName = describe "getName" [
    test "int"
    <| \_ -> Expect.equal "int"
    <| getName <| int "int"
  , test "string"
    <| \_ -> Expect.equal "string"
    <| getName <| string "string"
  , test "enum"
    <| \_ -> Expect.equal "enum"
    <| getName <| enum "enum" []
  , test "regex"
    <| \_ -> Expect.equal "regex"
    <| getName <| regex "regex" ""
  ]

testToString : Test
testToString = let
    arguments = Dict.fromList [
      ("int", "42"),
      ("string", "param"),
      ("enum", "lion"),
      ("regex", "123-abc")
    ]
    toString_ = Arguments.toString arguments
  in describe "toString" [
    test "int"
    <| \_ -> Expect.equal (Ok "42")
    <| toString_ <| int "int"
  , test "string"
    <| \_ -> Expect.equal (Ok "param")
    <| toString_ <| string "string"
  , test "enum"
    <| \_ -> Expect.equal (Ok "lion")
    <| toString_ <| enum "enum" ["lion", "bear"]
  , test "regex"
    <| \_ -> Expect.equal (Ok "123-abc")
    <| toString_ <| regex "regex" "\\d{3}-\\w{3}$"
  ]

testToStringFail : Test
testToStringFail = let
    arguments = Dict.fromList [
      ("int", "noint"),
      ("string", "s/d"),
      ("enum", "penguin"),
      ("regex", "abc-123")
    ]
    toString_ = Arguments.toString arguments
  in describe "toString" [
    test "int"
    <| \_ -> Expect.equal (Err "wrong argument: int")
    <| toString_ <| int "int"
  , test "string"
    <| \_ -> Expect.equal (Err "wrong argument: string")
    <| toString_ <| string "string"
  , test "enum"
    <| \_ -> Expect.equal (Err "wrong argument: enum")
    <| toString_ <| enum "enum" ["lion", "bear"]
  , test "regex"
    <| \_ -> Expect.equal (Err "wrong argument: regex")
    <| toString_ <| regex "regex" "\\d{3}-\\w{3}$"
  ]
