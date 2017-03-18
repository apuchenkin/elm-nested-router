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
import Html             exposing (Html)
import Html.Events      exposing (onWithOptions)
import Html.Attributes  as Attr
import Json.Decode      as Json

import Matcher.Matcher   as Matcher exposing (Route, URL)
import Matcher.Arguments as Arguments
import Router.Types        exposing (..)

getPath : RouterConfig route state msg -> Location -> URL
getPath config location =
    let
      (RouterConfig c) = config
      urlPath = if c.html5
        then location.pathname
        else Maybe.withDefault "/" <| Maybe.map Tuple.second <| String.uncons location.hash
    in
      if c.removeTrailingSlash then Matcher.removeTrailingSlash urlPath else urlPath

{-| Decomposes Route to string. Exposed by `Router` -}
buildUrl : RouterConfig route state msg -> Route route -> String
buildUrl routerConfig route =
  let
    (RouterConfig config) = routerConfig
    url = Matcher.buildURL config.routeConfig route
    url_new = if config.removeTrailingSlash then Matcher.removeTrailingSlash url else url
  in if config.html5 then url_new else String.cons Arguments.hash url_new

{-| binds forward action to existing HTML attributes. Exposed by `Router` -}
bindForward : RouterConfig route state msg -> Route route -> List (Html.Attribute (Msg route msg)) -> List (Html.Attribute (Msg route msg))
bindForward config route attrs =
  let
    options = {stopPropagation = True, preventDefault = True}
    -- action = forward config matcher route
  in
    Attr.href (buildUrl config route)
    :: onWithOptions "click" options (Json.succeed <| Forward route)
    :: attrs

{-| Preforms a transition to provided `Route`. Exposed by `Router` -}
forward : RouterConfig route state msg -> Route route -> Cmd (Msg route msg)
forward config = Navigation.newUrl << buildUrl config

{-| Redirects to provided `Route`. Exposed by `Router` -}
redirect : RouterConfig route state msg -> Route route -> Cmd (Msg route msg)
redirect config = Navigation.modifyUrl << buildUrl config
