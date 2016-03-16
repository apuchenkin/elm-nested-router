module Router.Matcher where

import Regex
import String
import List.Extra

import Dict               exposing (Dict)
import MultiwayTreeUtil   exposing (treeLookup, forestLookup, traverse)
import MultiwayTree       exposing (Tree (..), Forest, datum, children)

import Combine        exposing (Parser, many1, parse, many, while, between, end, rec, manyTill)
import Combine.Char   exposing (char, noneOf, anyChar)
import Combine.Infix  exposing ((<$>), (*>), (<*), (<*>), (<|>))
import Combine.Num

import Router.Types    exposing (..)
import Router.Helpers  exposing (singleton, combineParams)

paramChar : Char
paramChar = ':'

-- left delimiter
ld : Char
ld = '['

-- right delimiter
rd : Char
rd = ']'

stringParser : Parser String
stringParser = String.fromList <$> many1 (noneOf [ '/', paramChar, '#', '?', ld, rd ])

paramParser : Parser String
paramParser = char paramChar *> stringParser

paramsParser : Parser (List String)
paramsParser = many <| while ((/=) paramChar) *> paramParser <* while ((/=) paramChar)

getParams : String -> List String
getParams string = case fst <| parse paramsParser string of
  Err _     -> Debug.crash "getParams : String -> List String"
  Ok param  -> param

-- unwrapCache : Automaton String (List String)
-- unwrapCache = Automaton.hiddenState Dict.empty <| \url state -> case Dict.get url state of
--   Just value -> (value, state)
--   Nothing ->
--     let result = unwrap url
--     in (result, Dict.insert url result state)

{-| @Private
  Unwraps string that contains brackets to a list of strings without brackets
-}
unwrap : String -> List String
unwrap raw =
  let
    regex   = Regex.regex "^(.*)\\[([^\\]\\[]+)\\](.*)$"
    matches = Regex.find (Regex.AtMost 1) regex raw
    result = case matches of
      []         -> [raw]
      [match]    -> case match.submatches of
        [Just a, Just b, Just c] -> List.concat <| List.map unwrap [a ++ b ++ c, a ++ c]
        _ -> [raw]
      _ -> Debug.crash "result = case matches of _"

  in List.reverse <| List.sortBy String.length <| List.Extra.dropDuplicates <| result

parseUrlParams : RawURL -> Dict String Constraint -> URL -> (Result (List String) RouteParams, String)
parseUrlParams raw constraints url =
  let
    params = getParams raw
    strings = case params of
      [] -> [raw]
      p  -> Regex.split Regex.All (Regex.regex <| String.join "|" <| List.map (String.cons paramChar) p) raw

    sringParsers = List.map Combine.string        strings
    paramParsers = List.map (\param -> Maybe.withDefault stringParser <| Maybe.map (\c -> case c of
        Int       -> toString <$> Combine.Num.int
        Enum list -> Combine.choice <| List.map Combine.string list
        Regex reg -> Combine.regex reg
        _         -> stringParser
      ) <| Dict.get param constraints) params

    last = case List.Extra.last sringParsers of
      Nothing -> Debug.crash "List.Extra.last sringParsers"
      Just v  -> v

    parsers = List.map2 (\p1 p2 -> p1 *> p2) sringParsers paramParsers
    parser = (List.foldr (\p pacc -> p `Combine.andThen` (\r -> (++) r <$> pacc))
       (singleton <$> last)
       (List.map (Combine.map singleton) parsers)
       ) -- <* Combine.end

    (result, context) = Combine.parse parser url
    zipValues values = Dict.fromList <| List.map2 (,) params values
  in (Result.map zipValues result, context.input)

matchRaw : (route -> (List RawURL, Dict String Constraint)) -> Forest route -> URL -> Maybe (Route route)
matchRaw rawRoute forest url = List.head <| List.filterMap (\tree ->
    let (raws, constraints) = rawRoute <| datum tree
    in List.head <| List.filterMap (\pattern -> let (result, url') = parseUrlParams pattern constraints url
       in case result of
        Err _       -> Nothing
        Ok  dict    ->
          let
            child = matchRaw rawRoute (children tree) url'
            childRoute = Maybe.map (combineParams dict) child
          in case String.isEmpty url' of
              True  -> Just <| Maybe.withDefault (datum tree, dict) childRoute
              False -> childRoute
      ) raws
    ) forest

match : (route -> (RawURL, Dict String Constraint)) -> Forest route -> URL -> Maybe (Route route)
match rawRoute forest url = matchRaw ((\(r,c) -> (unwrap r, c)) << rawRoute) forest url

buildRawUrl : List RawURL -> Route route -> URL
buildRawUrl raws (route, params) =
  let
    urls = List.map (\raw -> Dict.foldl (\param value string -> Regex.replace
            (Regex.AtMost 1)
            (Regex.regex <| paramChar `String.cons` param)
            (always value)
            string
          ) raw params
      ) raws
    urls' = List.filter (not << String.contains (String.fromChar paramChar)) urls

  in case List.head urls' of
    Nothing -> Debug.crash <| "not enough params to build URL: " ++ toString route
    Just url -> url

composeRawUrl : (route -> RawSegment) -> Forest route -> route -> RawURL
composeRawUrl rawRoute forest route =
  let
    zipper = forestLookup route forest
    path   = Maybe.withDefault [] <| Maybe.map traverse zipper
    segments = List.map rawRoute path
  in List.foldl (flip (++)) "" segments

-- decompose Route to string
buildUrl : (route -> RawSegment) -> Forest route -> Route route -> URL
buildUrl rawRoute forest (route, params) =
  let raws = unwrap <| composeRawUrl rawRoute forest route
  in buildRawUrl raws (route, params)

-- path from node a to node b in the forest
getPath : a -> Forest a -> List a
getPath route forest = Maybe.withDefault []
  <| flip Maybe.map (List.head <| List.filterMap (\tree -> treeLookup route tree) forest)
  <| \zipper -> traverse zipper

mapParams : (route -> RawSegment) -> List route -> RouteParams -> List (Route route)
mapParams rawRoute routes params = flip List.map routes <| \route ->
    let p = getParams (rawRoute route)
    in (route, Dict.filter (\k _ -> List.member k p) params)

hasTrailingSlash : URL -> Bool
hasTrailingSlash url = case String.right 1 url of
  "/" -> True
  _ -> False

removeTrailingSlash : URL -> URL
removeTrailingSlash url = if hasTrailingSlash url then String.dropRight 1 url else url
