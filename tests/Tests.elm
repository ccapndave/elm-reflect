module Tests exposing (..)

import Test exposing (..)
import Expect
import Dict
import Json.Decode as JD
import Reflect exposing (..)


all : Test
all =
  describe "Reflection Test Suite"
    [ typeToNameTest
    , typeToParametersTest
    , typeParameterRecordToDictTest
    , typeParameterRecordToIntTest
    ]


type Type
  = JustAType
  | ATypeWithAnInt Int
  | ATypeWithAStringRecord { a : String, b : String }
  | ATypeWithAnIntRecord { a : Int, b: Int }


typeToNameTest : Test
typeToNameTest =
  describe "typeToNameTest" <|
    [ test "justAType" <|
        \() ->
          Expect.equal (typeToName JustAType) "JustAType"
    , test "aTypeWithAnInt" <|
        \() ->
          Expect.equal (typeToName <| ATypeWithAnInt 100) "ATypeWithAnInt"
    ]


typeToParametersTest : Test
typeToParametersTest =
  describe "typeToParameters" <|
    [ test "justAType" <|
        \() ->
          Expect.equal (typeToParameters JustAType) "JustAType"
    , test "aTypeWithAnInt" <|
        \() ->
          Expect.equal (typeToParameters <| ATypeWithAnInt 100) "100"
    , test "aTypeWithAStringRecord" <|
        \() ->
          Expect.equal (typeToParameters <| ATypeWithAStringRecord { a = "a", b = "b" }) """{ a = "a", b = "b" }"""
    ]


typeParameterRecordToDictTest : Test
typeParameterRecordToDictTest =
  describe "typeParameterRecordToDict" <|
    [ test "valid" <|
        \() ->
          Expect.equal
            (typeParameterRecordToDict JD.string <| ATypeWithAStringRecord { a = "a", b = "b" })
            (Ok <| Dict.fromList [ ("a", "a"), ("b", "b")])
    , test "invalid" <|
        \() ->
          Expect.equal
            (typeParameterRecordToDict JD.string <| ATypeWithAnInt 100)
            (Err "Expecting an object but instead got: 100")
    ]


typeParameterRecordToIntTest : Test
typeParameterRecordToIntTest =
  describe "typeParameterToInt" <|
    [ test "valid" <|
        \() ->
          Expect.equal
            (typeParameterToInt <| ATypeWithAStringRecord { a = "a", b = "b" })
            (Err "Given an invalid JSON: Unexpected token a in JSON at position 2")
    , test "invalid" <|
        \() ->
          Expect.equal
            (typeParameterToInt <| ATypeWithAnInt 100)
            (Ok 100)
    ]
