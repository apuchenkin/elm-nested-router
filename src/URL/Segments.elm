module URL.Segments exposing (
    Segment,
    end, static, int, string, enum, regex, maybe, combine, (</>),
    getConstraints, toString, parse
  )

{-| URL segments

# Types
@docs Segment

# Constructors
@docs end, static, int, string, enum, regex

# Combinators
@docs maybe, combine, (</>)

# Utils
@docs getConstraints, toString, parse
-}

import Dict

import Combine exposing (Parser, (<$>), (*>), (<*), (<*>), (<|>), (<$))
import Combine.Char

import URL.Arguments as Arguments exposing (Name, Arguments)

{-| A part of an URL -}
type Segment = Terminator | Static String | Argument Arguments.Constraint | Sequence Segment Segment | Optional Segment

{-| URL end -}
end : Segment
end = Terminator

{-| Static segment of URL -}
static : String -> Segment
static = Static

{-| URL arguments that matches integers -}
int : Name -> Segment
int = Argument << Arguments.int

{-| URL arguments that matches string characters -}
string : Name -> Segment
string = Argument << Arguments.string

{-| URL arguments that matches one of provided strings -}
enum : Name -> List String -> Segment
enum name options = Argument <| Arguments.enum name options

{-| URL arguments that matches provided regexp -}
regex : Name -> String -> Segment
regex name reg = Argument <| Arguments.regex name reg

{-| Marks segment as optional -}
maybe : Segment -> Segment
maybe segment = Optional segment

{-| Combines two URL segments -}
combine : Segment -> Segment -> Segment
combine s1 s2 = case s1 of
  Terminator -> s2
  Sequence s Terminator -> Sequence s s2
  _ -> Sequence s1 s2

{-| Infix version of `combine` -}
(</>) : Segment -> Segment -> Segment
(</>) = combine
infixl 8 </>

{-| Returns list of constraints, of a given segment -}
getConstraints : Segment -> List Arguments.Constraint
getConstraints segment = case segment of
  Terminator -> []
  Static string -> []
  Argument constraint -> [constraint]
  Optional segment -> getConstraints segment
  Sequence s1 s2 -> getConstraints s1 ++ getConstraints s2

joinStrings : List String -> String
joinStrings strings = String.concat
  <| List.intersperse "/"
  <| List.filter (not << String.isEmpty) strings

combineResults : List (Result error value) -> Result (List error) (List value)
combineResults =
  let
    step result acc = case result of
      Err err -> Err [err]
      Ok value -> Result.map ((::) value) acc
  in List.foldr step (Ok [])

{-| Returns a string builded on the base of segment and its arguments -}
toString : Arguments -> Segment -> Result String String
toString arguments segment =
  case segment of
  Terminator -> Ok ""
  Static string -> Ok string
  Argument constraint -> Arguments.toString arguments constraint
  Optional segment -> Ok <| Result.withDefault "" <| toString arguments segment
  Sequence s1 s2 -> Result.map joinStrings
    <| Result.mapError (String.concat << List.intersperse "/")
    <| combineResults
    <| List.map (toString arguments) [s1, s2]

slashParser : Parser s ()
slashParser = Combine.skip <| Combine.Char.char Arguments.slash

terminatorParser : Parser s ()
terminatorParser = slashParser <|> Combine.end

getParser : Segment -> List (Parser error Arguments)
getParser segment = case segment of
  Terminator -> [Dict.empty <$ terminatorParser]
  Static string -> [Dict.empty <$ Combine.string string]
  Argument constraint -> [Arguments.getParser constraint]
  Optional segment -> Combine.succeed Dict.empty :: getParser segment
  Sequence s1 s2 -> let starters = case s2 of
      Optional s -> Combine.succeed Dict.empty :: List.map ((*>) terminatorParser) (getParser s2)
      _ -> List.map ((*>) terminatorParser) (getParser s2)
    in List.concat <| List.map (\parser -> List.map ((<*>) (Dict.union <$> parser)) starters) (getParser s1)

{-| Attempts to parse a string (URL part) into Result -}
parse : String -> Segment -> Result (Combine.ParseErr ()) (Combine.ParseOk () Arguments)
parse = flip <| Combine.parse << Combine.choice << getParser
