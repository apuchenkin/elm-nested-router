module URL.Matcher exposing (..)

import URL.Arguments as Arguments exposing (Arguments)
import URL.Segments as Segments exposing ((</>))

type alias URL = String

{-| combined abstract route type with params -}
type alias Route route = {
    route: route
  , arguments: Arguments
  -- , query: Arguments
  }

type alias RouteConfig route = {
    segment: Segments.Segment
  , parent: Maybe route
  }

type alias Sitemap route = List route

type alias GetConfig route = route -> RouteConfig route

routeConfig : Maybe route -> Segments.Segment -> RouteConfig route
routeConfig parent segment = {
    segment = segment
  , parent = parent
  }

(//>) : Maybe route -> Segments.Segment -> RouteConfig route
(//>) = routeConfig
infixl 6 //>

isChild : GetConfig route -> Maybe route -> route -> Bool
isChild getConfig parent child = parent == (.parent << getConfig) child

childs : GetConfig route -> List route -> Maybe route -> List route
childs getConfig routes route = List.filter (isChild getConfig route) routes

parents : GetConfig route -> List route -> route -> List route
parents getConfig routes route = case (.parent << getConfig) route of
  Nothing -> []
  Just parent -> parents getConfig routes parent ++ [parent]

hasTrailingSlash : URL -> Bool
hasTrailingSlash url = case String.right 1 url of
    "/" -> True
    _ -> False

removeTrailingSlash : URL -> URL
removeTrailingSlash url = if hasTrailingSlash url then String.dropRight 1 url else url

removeLeadingSlash : URL -> URL
removeLeadingSlash url = case String.left 1 url of
    "/" -> String.dropLeft 1 url
    _ -> url

matchOne : GetConfig route -> List route -> URL -> route -> Maybe (Route route)
matchOne getConfig routes url route =
  let
    config = getConfig route
    segments = List.map (.segment << getConfig) <| parents getConfig routes route
    segment = List.foldr (</>) Segments.end <| segments ++ [config.segment]
    result = Segments.parse (removeLeadingSlash url) segment
  in case result of
    Err err -> Nothing
    Ok (_, _, arguments) -> Just {
      route = route
    , arguments = arguments
    }

match : GetConfig route -> Sitemap route -> URL -> Maybe (Route route)
match getConfig sitemap url = List.head
  <| List.filterMap identity
  <| List.map (matchOne getConfig sitemap url)
  <| sitemap

composeURL : GetConfig route -> Route route -> URL
composeURL getConfig {route, arguments} = let
    config = getConfig route
    url = case Segments.toString arguments config.segment of
      Err err -> Debug.crash err
      Ok url -> url
  in case config.parent of
    Nothing -> url
    Just parent -> String.concat
      <| List.intersperse "/"
      <| List.filter (not << String.isEmpty)
      <| [composeURL getConfig { route = parent, arguments = arguments }, url]

buildURL : GetConfig route -> Route route -> URL
buildURL getConfig route = String.cons Arguments.slash <| composeURL getConfig route

route : route -> Arguments -> Route route
route route arguments = { route = route, arguments = arguments }
