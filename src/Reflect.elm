module Reflect exposing
  ( typeToName
  , typeToParameters
  , typeParameterRecordToDict
  , typeParameterToInt
  )

{-| In your face, type system!  This module provides some very un-Elmy run-type introspection on type parameters by hacking around
with `toString`, regular expressions and Json decoders.

In general these functions are all very horrible and should never be used, but there are a few specialized cases (probably mostly
based around situations where you want to maintain type-safety for stuff that is loaded at runtime) that it can prove useful.

For a real-world example of this package being used, check out the `ccapndave/elm-translator` package.

@docs typeToName, typeToParameters, typeParameterRecordToDict, typeParameterToInt
-}

import Json.Decode as JD exposing (Decoder)
import Regex exposing (regex)
import Dict exposing (Dict)

{-| Get the base name of a type as a string, discarding any type parameters.

    type Literal
      = MyDetails { age : Int, height : Int }

    typeToName (MyDetails { age = 30, height = 158 }) --> "MyDetails"
-}
typeToName : a -> String
typeToName typeLiteral =
  typeLiteral
    |> toString
    |> String.split " "
    |> List.head
    |> Maybe.withDefault ""


{-| Get the type parameters of a type as a string, discarding the identifier.

    type Literal
      = MyDetails { age : Int, height : Int }

    typeToParameters (MyDetails { age = 30, height = 158 }) --> "{ age = 30, height = 158 }"
-}
typeToParameters : a -> String
typeToParameters typeLiteral =
  typeLiteral
    -- String ~~> turn the type into a String using the native toString implementation
    |> toString
    -- String ~~> Get rid of the name of the type, just leaving the parameters
    |> Regex.replace (Regex.AtMost 1) (regex "^\\w* ") (always "")


{-| Turn a parameter which is a record into a Dict.

For example, if you have a type:

    type Literal
      = MyDetails { age : Int, height : Int }

Then you could use this function to get the type parameter as a `Dict String Int` with:

    typeParametersToDict Json.Decode.int <| MyDetails { age = 30, height = 158 }

Note that this will only work when there is a single type parameter which is a record, and all the elements of the record have to
be of the same type.  You can choose the type using the decoder parameter, but probably only simple types will work (string, int,
float, boolean).
-}
typeParameterRecordToDict : Decoder b -> a -> Result String (Dict String b)
typeParameterRecordToDict decoder typeLiteral =
  typeToParameters typeLiteral
    -- String ~~> Use a regexp to turn the parameters into valid JSON by turning { a = "hello" } into { "a": "hello" }
    |> Regex.replace Regex.All (regex "(\\w*) =")
      (\{ submatches } ->
        submatches
          |> List.filterMap identity
          |> List.head
          |> Maybe.map (\id -> "\"" ++ id ++ "\"" ++ ":")
          |> Maybe.withDefault "" -- this can't happen in valid JSON
      )
    -- String ~~> Use the dict decoder to turn this into a Dict of whatever Decoder we gave as a parameter
    |> JD.decodeString (JD.dict decoder)


{-| Turn a parameter which is a record into a Int.

For example, if you have a type:

    type Literal
      = Age Int

Then you could use this function to get the type parameter as a `Int` with:

    typeParameterToInt <| Age 30
-}
typeParameterToInt : a -> Result String Int
typeParameterToInt typeLiteral =
  typeToParameters typeLiteral
    -- String ~~> Use the int decoder to turn this into an Int
    |> JD.decodeString JD.int
