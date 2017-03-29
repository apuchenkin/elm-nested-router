module URL.Matcher exposing (
    URL,
    hasTrailingSlash, removeTrailingSlash,
    match, buildURL
  )

{-| Module for working with URLs

# Types
@docs URL

# Utility functions
@docs hasTrailingSlash, removeTrailingSlash

# Main functions
@docs match, buildURL
-}

import URL.Route as Route exposing (..)
import URL.Arguments as Arguments exposing (Arguments)
import URL.Segments as Segments exposing (Segment, (</>))

{-| An URL -}
type alias URL = String

{-| Checks whether URL has trailing slash -}
hasTrailingSlash : URL -> Bool
hasTrailingSlash url = case String.right 1 url of
    "/" -> True
    _ -> False

{-| Removes trailing slash from URL if any -}
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
    Ok (_, _, arguments) -> Just <| Route.route route arguments

{-| Tries to match given URL into route -}
match : GetConfig route -> List route -> URL -> Maybe (Route route)
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
      <| [composeURL getConfig (Route.route parent arguments), url]

{-| Build URL for specified route -}
buildURL : GetConfig route -> Route route -> URL
buildURL getConfig route = String.cons Arguments.slash <| composeURL getConfig route
