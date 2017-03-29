module Router.Navigation exposing (..)

{-|  @Private

Navigation module

A set of functions that is not testable due to "Navigation" import
see:
  - https://github.com/elm-lang/navigation/issues/7
  - https://github.com/elm-lang/dom/issues/2
-}

import String
import Navigation       exposing (Location)

import URL.Route exposing (Route)
import URL.Matcher as Matcher exposing (URL)
import URL.Arguments as Arguments
import Router.Types exposing (RouterConfig (..))

{-| Decomposes Route to string. Exposed by `Router` -}
buildUrl : RouterConfig route state msg -> Route route -> String
buildUrl routerConfig route =
  let
    (RouterConfig config) = routerConfig
    url = Matcher.buildURL (.route << config.routeConfig) route
    url_new = if config.removeTrailingSlash then Matcher.removeTrailingSlash url else url
  in
    if config.html5 then url_new else String.cons Arguments.hash url_new

{-| Preforms a transition to provided `Route`. Exposed by `Router` -}
forward : RouterConfig route state msg -> Route route -> Cmd msg
forward config = Navigation.newUrl << buildUrl config

{-| Redirects to provided `Route`. Exposed by `Router` -}
redirect : RouterConfig route state msg -> Route route -> Cmd msg
redirect config = Navigation.modifyUrl << buildUrl config

match : RouterConfig route state msg -> URL -> Maybe (Route route)
match (RouterConfig config) = Matcher.match (.route << config.routeConfig) config.routes
