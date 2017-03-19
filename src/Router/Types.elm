module Router.Types exposing (..)

{-| Router types

# URL parts
@docs Msg

# Actions and handlers
@docs WithRouter, GetConfig, RouteConfig, Action, Transition, Render

# Router
@docs Router, RouterConfig, RouterState
-}

import Dict           exposing (Dict)
import Html           exposing (Html)
import Navigation

import Matcher.Matcher as Matcher exposing (Route, URL)
import Matcher.Arguments exposing (Arguments)

{-| `Action` represents function that prforms something with application state, and might contain side efects -}
type alias Action state msg = state -> (state, Cmd msg)

{-| `Action` represents function that prforms something with application state, and might contain side efects -}
type alias Render route state msg = state -> Dict String (Html (Msg route msg)) -> Dict String (Html (Msg route msg))

{-| `Action` represents function that prforms something with application state, and might contain side efects -}
type alias GetConfig route state msg = route -> RouteConfig route state msg

{-|
  A `Handler` is a piece of functionality binded to specific route
  * `render` &mdash; Function that describes how to render application state to map of named views
  * `actions` &mdash; A set of necessary to perform actions
-}
type alias RouteConfig route state msg = {
    route: Matcher.RouteConfig route
  , render: Render route state msg
  , actions: List msg
  }

{-| A state of router -}
type alias RouterState route = {
    route: Maybe route
  , arguments: Arguments
  }

{-| Type extension for the application state -}
type alias WithRouter route state = { state | router : RouterState route}

{-| A transition from route A to route B -}
type alias Transition route msg = Maybe (Route route) -> Maybe (Route route) -> List msg

{-| A state of router -}
type Msg route msg = AppMsg msg | Transition Navigation.Location | Forward (Route route) | Redirect (Route route)

{-|
  `RouterConfig` is configuration for the router:

  * `html5` &mdash; Use html5 pushState.
  * `removeTrailingSlash` &mdash; Trailing slashes will be removed from matched and builded urls
  * `layout` &mdash; Main rendered function that combines named views gathered from Handlers in a single HTML
  * `transition` &mdash; An action that should be executed on every router transition
  * `routeConfig` &mdash; A mapping between route and route configuration
  * `routes` &mdash; A list of routes available for routing
  * `subscriptions` &mdash; A list of subscriptions (see: [elm-lang/html](http://package.elm-lang.org/packages/elm-lang/html/1.1.0/Html-App) for details)
-}
type RouterConfig route state msg = RouterConfig {
    html5: Bool
  , removeTrailingSlash: Bool
  , update : msg -> Action state (Msg route msg)
  , layout: Router route state msg -> state -> Dict String (Html (Msg route msg)) -> (Html (Msg route msg))
  , onTransition: Router route state msg -> Transition route msg
  , routeConfig: GetConfig route state msg
  , routes: List route
  , subscriptions : state -> Sub (Msg route msg)
  }

{-|
  A `Router` is a provider of following functions:

  * `bindForward` &mdash; Binds a `forward` action to a provided `Route` with a list of html attributes.
    This is useful to create links in application
  * `buildUrl` &mdash; Builds an URL for provided `Route`
  * `forward` &mdash; Preforms a transition to provided `Route`
  * `redirect` &mdash; Redirects to provided `Route`
  * `match` &mdash; Performs attempt to match provided URL.

  Router also provide it's `config`
-}
type alias Router route state msg = {
    config : RouterConfig route state msg
  , bindForward : Route route -> List (Html.Attribute (Msg route msg)) -> List (Html.Attribute (Msg route msg))
  , buildUrl : Route route -> URL
  , forward : Route route -> Cmd (Msg route msg)
  , redirect : Route route -> Cmd (Msg route msg)
  , match : String -> Maybe (Route route)
  }
