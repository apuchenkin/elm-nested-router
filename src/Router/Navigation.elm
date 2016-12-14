module Router.Navigation exposing (..)

{-| @Private

Navigation module

A set of functions that is not testable due to "Navigation" import
see:
  - https://github.com/elm-lang/navigation/issues/7
  - https://github.com/elm-lang/dom/issues/2
-}

import String
import Navigation exposing (Location)
import Html exposing (Html)
import Html.Events exposing (onWithOptions)
import Html.Attributes as Attr
import Json.Decode as Json
import Router.Matcher as Matcher exposing (Matcher)
import Router.Types exposing (..)


{-| @Private
  Preforms attempt to match provided url to a route by a given routes configuration
-}
matchRoute : Matcher route state -> String -> Maybe (Route route)
matchRoute matcher url =
    matcher.match url


getPath : RouterConfig route state -> Location -> URL
getPath config location =
    let
        (RouterConfig c) =
            config

        urlPath =
            if c.html5 then
                location.pathname
            else
                Maybe.withDefault "/" <| Maybe.map Tuple.second <| String.uncons location.hash
    in
        if c.removeTrailingSlash then
            Matcher.removeTrailingSlash urlPath
        else
            urlPath


{-| Decomposes Route to string. Exposed by `Router`
-}
buildUrl : RouterConfig route state -> Matcher route state -> Route route -> String
buildUrl routerConfig matcher route =
    let
        (RouterConfig config) =
            routerConfig

        url =
            matcher.buildUrl route

        url_ =
            if config.removeTrailingSlash then
                Matcher.removeTrailingSlash url
            else
                url

        url__ =
            if config.html5 then
                url_
            else
                String.cons Matcher.hash url_
    in
        url__


{-| binds forward action to existing HTML attributes. Exposed by `Router`
-}
bindForward : RouterConfig route state -> Matcher route state -> Route route -> List (Html.Attribute (Action state)) -> List (Html.Attribute (Action state))
bindForward config matcher route attrs =
    let
        options =
            { stopPropagation = True, preventDefault = True }

        action =
            forward config matcher route
    in
        Attr.href (buildUrl config matcher route)
            :: onWithOptions "click" options (Json.succeed action)
            :: attrs


{-| Preforms a transition to provided `Route`. Exposed by `Router`
-}
forward : RouterConfig route state -> Matcher route state -> Route route -> Action state
forward routerConfig matcher route state =
    let
        -- (RouterConfig config) = routerConfig
        url =
            buildUrl routerConfig matcher route

        msg =
            Navigation.newUrl url
    in
        Response ( state, msg )


{-| Redirects to provided `Route`. Exposed by `Router`
-}
redirect : RouterConfig route state -> Matcher route state -> Route route -> Action state
redirect routerConfig matcher route state =
    let
        -- (RouterConfig config) = routerConfig
        url =
            buildUrl routerConfig matcher route

        msg =
            Navigation.modifyUrl url
    in
        Response ( state, msg )
