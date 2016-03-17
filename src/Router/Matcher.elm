module Router.Matcher where

import Regex
import String
import List.Extra
import Dict               exposing (Dict)

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

matchInternal : (String -> List String) -> (route -> RouteConfig route state) -> List route -> List route -> URL -> Maybe (Route route)
matchInternal unwrap getConfig routes pool url = List.foldl (\route match ->
  case match of
    Just _ -> match
    Nothing ->
      let
        config = getConfig route
        raws = unwrap config.segment
      in List.foldl (\raw match' ->
        case match' of
          Just _ -> match'
          Nothing -> let
              (result, url') = parseUrlParams raw config.constraints url
            in
              case result of
              Err _       -> Nothing
              Ok  dict    ->
                let matchChildren route =
                  let
                    (childrens, pool') = filterParent (.parent << getConfig) (Just route) pool
                    child = matchInternal unwrap getConfig childrens pool' url'
                    childRoute = Maybe.map (combineParams dict) child
                  in childRoute
                in case config.bypass of
                True -> matchChildren route
                False -> case String.isEmpty url' of
                  True  -> Just (route, dict)
                  False -> matchChildren route
      ) Nothing raws
  ) Nothing routes

filterParent : (route -> Maybe route) -> Maybe route -> List route -> (List route, List route)
filterParent getParent route routes =
  List.foldl (\r (a,b) -> if getParent r == route then (a ++ [r], b) else (a, b ++ [r])) ([],[]) routes

match' : (String -> List String) -> (route -> RouteConfig route state) -> List route -> URL -> Maybe (Route route)
match' unwrap getConfig routes url =
  let
    (roots, pool) = filterParent (.parent << getConfig) Nothing routes
  in matchInternal unwrap getConfig roots pool url

match : (route -> RouteConfig route state) -> List route -> URL -> Maybe (Route route)
match getConfig routes url = match' unwrap getConfig routes url

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

composeRawUrl : (route -> RawSegment) -> (route -> Maybe route) -> route -> RawURL
composeRawUrl getSegment getParent route =
  let
    segments = List.map getSegment <| getPath getParent route
  in
    List.foldl (flip (++)) "" segments

-- decompose Route to string
buildUrl : (route -> RawSegment) -> (route -> Maybe route) -> Route route -> URL
buildUrl getSegment getParent (route, params) =
  let raws = unwrap <| composeRawUrl getSegment getParent route
  in buildRawUrl raws (route, params)

getPathInternal : (route -> Maybe route) -> route -> List route -> List route
getPathInternal getParent route acc = case getParent route of
  Nothing -> route :: acc
  Just parent -> route :: getPathInternal getParent parent acc

-- traverses route up to top parent
getPath : (route -> Maybe route) -> route -> List route
getPath getParent route = List.reverse <| getPathInternal getParent route []

mapParams : (route -> RawSegment) -> List route -> RouteParams -> List (Route route)
mapParams getSegment routes params = flip List.map routes <| \route ->
    let p = getParams (getSegment route)
    in (route, Dict.filter (\k _ -> List.member k p) params)

hasTrailingSlash : URL -> Bool
hasTrailingSlash url = case String.right 1 url of
  "/" -> True
  _ -> False

removeTrailingSlash : URL -> URL
removeTrailingSlash url = if hasTrailingSlash url then String.dropRight 1 url else url
