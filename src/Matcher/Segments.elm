module Matcher.Segments exposing (..)

import Matcher.Arguments as Arguments exposing (Name)

import Combine exposing (Parser, many1, parse, many, while, between, end, manyTill, (<$>), (*>), (<*), (<*>), (<|>))
import Combine.Char

type Segment = Terminator | Str String | Argument Arguments.Constraint | Sequence (List Segment)

end : Segment
end = Terminator

str : String -> Segment
str = Str

int : Name -> Segment
int = Argument << Arguments.int

string : Name -> Segment
string = Argument << Arguments.string

enum : Name -> List String -> Segment
enum name options = Argument <| Arguments.enum name options

regex : Name -> String -> Segment
regex name reg = Argument <| Arguments.regex name reg

combine : Segment -> Segment -> Segment
combine s1 s2 = Sequence [s1, s2]

(</>) : Segment -> Segment -> Segment
(</>) = combine
infixl 8 </>

toString : Arguments.Arguments -> Segment -> String
toString args segment = case segment of
  Terminator -> ""
  Str string -> string
  Argument constraint -> Arguments.constraintToString args constraint
  Sequence list -> List.foldl (++) "" (List.map (toString args) list)

getParser : Segment -> Parser s String
getParser segment = case segment of
  Terminator -> always "" <$> Combine.end
  Str string -> Combine.string string
  Argument constraint -> Arguments.getParser constraint
  Sequence [] -> Combine.fail "empty sequence"
  Sequence (head::tail) -> List.foldl (\segment parser -> parser
    |> Combine.andThen (\r -> always r <$> Combine.Char.char Arguments.slash)
    |> Combine.andThen (\r -> (++) r <$> getParser segment))
   (getParser head)
   tail

parse : String -> Segment -> Result (Combine.ParseErr ()) (Combine.ParseOk () String)
parse url segment =
  let parser = getParser segment
  in Combine.parse parser url


-- parseUrlParams : RawURL -> Dict String Constraint -> URL -> Result (Combine.ParseErr ()) (RouteParams, String)
-- parseUrlParams raw constraints url =
--   let
--     params = getParams raw
--     strings = case params of
--       [] -> [raw]
--       p  -> Regex.split Regex.All (Regex.regex <| String.join "|" <| List.map (String.cons paramChar) p) raw
--
--     sringParsers = List.map Combine.string        strings
--     paramParsers = List.map (\param -> Maybe.withDefault stringParser <| Maybe.map (\c -> case c of
--         Int       -> toString <$> Combine.Num.int
--         Enum list -> Combine.choice <| List.map Combine.string list
--         Regex reg -> Combine.regex reg
--         _         -> stringParser
--       ) <| Dict.get param constraints) params
--
--     last = case List.Extra.last sringParsers of
--       Nothing -> Debug.crash "List.Extra.last sringParsers"
--       Just v  -> v
--
--     parsers = List.map2 (\p1 p2 -> p1 *> p2) sringParsers paramParsers
--     parser = (List.foldr (\p pacc -> p |> Combine.andThen (\r -> (++) r <$> pacc))
--        (singleton <$> last)
--        (List.map (Combine.map singleton) parsers)
--        ) -- <* Combine.end
--
--     res = Combine.parse parser url
--
--     zipValues (_, stream, values) = (Dict.fromList <| List.map2 (,) params values, stream.input)
--   in (Result.map zipValues res)
