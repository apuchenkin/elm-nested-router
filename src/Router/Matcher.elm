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
import Router.Helpers  exposing (singleton, combineParams, memoFallback)

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

getParams : String -> List Param
getParams string = case fst <| parse paramsParser string of
  Err _     -> Debug.crash "getParams : String -> List String"
  Ok param  -> param

{-| @Private
  Unwraps string that contains brackets to a list of strings without brackets
  TODO: check perfomance without regexp
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

-- TODO: check perfomance without regexp
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

-- TODO: check perfomance without regexp
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
  let
    raws = unwrap <| composeRawUrl getSegment getParent route
  in
    buildRawUrl raws (route, params)

getPathInternal : (route -> Maybe route) -> route -> List route -> List route
getPathInternal getParent route acc = case getParent route of
  Nothing -> route :: acc
  Just parent -> route :: getPathInternal getParent parent acc

-- traverses route up to top parent
getPath : (route -> Maybe route) -> route -> List route
getPath getParent route = List.reverse <| getPathInternal getParent route []

-- Maps a list of route params over list of routes
mapParams : Matcher route state -> List route -> RouteParams -> List (Route route)
mapParams matcher routes params = flip List.map routes <| \route ->
  (route, Dict.filter (\param _ -> List.member param (matcher.routeParams route)) params)

hasTrailingSlash : URL -> Bool
hasTrailingSlash url = case String.right 1 url of
    "/" -> True
    _ -> False

removeTrailingSlash : URL -> URL
removeTrailingSlash url = if hasTrailingSlash url then String.dropRight 1 url else url

{-| @Private
  Returns a set of handlers applicable to transtition between "from" and "to" routes.
-}
routeDiff : Matcher route state -> Maybe (Route route) -> Route route -> List route
routeDiff matcher from to =
  let
    getConfig = matcher.getConfig
    getParent = .parent << getConfig

    fromRoute = Maybe.map fst from
    fromParams = Maybe.withDefault Dict.empty <| Maybe.map snd from
    toRoute = fst to
    toParams = snd to

    fromPath = Maybe.withDefault [] <| Maybe.map matcher.traverse fromRoute
    toPath = matcher.traverse toRoute
    path = List.map2 (,) fromPath toPath

    fromPath' = mapParams matcher fromPath fromParams
    toPath'   = mapParams matcher toPath toParams

    commons = List.length
      <| List.Extra.takeWhile (uncurry (==))
      <| List.map2 (,) fromPath' toPath'

  in List.drop commons toPath

type alias Matcher route state = {
    getConfig: route -> RouteConfig route state
  , buildUrl: Route route -> URL
  , match: URL -> Maybe (Route route)
  , traverse: route -> List route
  , routeParams: route -> List Param
  , stringToRoute: String -> route
  , sids: List String
  }

-- matcher creates an object that provides a memoized versions of Matcher functions
matcher : RouterConfig route state -> Matcher route state
matcher (RouterConfig config) =
  let
    routes = config.routes
    getSegment = .segment << config.routeConfig
    getParent = .parent << config.routeConfig
    segments = List.map getSegment routes

    urls = List.map composeUrl' routes
    sids = List.map toString routes
    dict = Dict.fromList <| List.map2 (,) sids routes
    stringToRoute sid = case Dict.get sid dict of
      Just route -> route
      Nothing -> Debug.crash <| "stringToRoute: " ++ sid

    composeUrl    = memoFallback (\sid -> composeRawUrl getSegment getParent (stringToRoute sid)) sids
    getConfig     = memoFallback (\sid -> config.routeConfig (stringToRoute sid)) sids
    traverse      = memoFallback (\sid -> getPath getParent (stringToRoute sid)) sids
    unwrap'       = memoFallback unwrap (urls ++ segments)
    routeParams   = memoFallback (getParams << getSegment << stringToRoute) sids

    composeUrl' = composeUrl << toString
    getConfig' = getConfig << toString
    traverse' = traverse << toString
    routeParams' = routeParams << toString
    buildUrl' route =
      let raws = unwrap' <| composeUrl' (fst route)
      in buildRawUrl raws route
  in
    {
      getConfig = getConfig'
    , buildUrl = buildUrl'
    , match = match' unwrap' getConfig' routes
    , traverse = traverse'
    , routeParams = routeParams'
    , stringToRoute = stringToRoute
    , sids = sids
    }
