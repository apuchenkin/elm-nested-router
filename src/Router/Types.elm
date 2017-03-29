module Router.Types exposing (
    RouterState, WithRouter,
    Router, RouterConfig (..), RouteConfig, Views
  )

{-| Router types

# Router state
@docs RouterState, WithRouter

# Router and configs
@docs Router, RouterConfig, RouteConfig, Views
-}

import Dict           exposing (Dict)
import Html           exposing (Html)

import URL.Route as Route exposing (Route)
import URL.Matcher as Matcher exposing (URL)
import URL.Arguments exposing (Arguments)

{-| A state of router -}
type alias RouterState route = {
    route: Maybe route
  , arguments: Arguments
  }

{-| Type extension for the application state -}
type alias WithRouter route state = { state | router : RouterState route}

{-| Collection of named views -}
type alias Views msg = Dict String (Html msg)

{-|
  `RouteConfig` is a configuration related to specific route
  * `route` &mdash; Describes route structure (see: [`URL.Route.Config`][doc] for details)
  * `render` &mdash; Function that describes how to render application state as named views
  * `actions` &mdash; List of messages to execute

  [doc]: http://package.elm-lang.org/packages/apuchenkin/elm-nested-router/latest/URL-Route#Config
-}
type alias RouteConfig route state msg = {
    route: Route.Config route
  , render: Router route state msg -> state -> Views msg -> Views msg
  , actions: List msg
  }

{-|
  `RouterConfig` is configuration for the router:

  * `html5` &mdash; Use html5 pushState.
  * `removeTrailingSlash` &mdash; Trailing slashes will be removed from matched and builded urls
  * `routes` &mdash; A list of routes available for routing
  * `routeConfig` &mdash; A mapping between route and route configuration
  * `update` &mdash; Function that describes how to handle specific application message
  * `layout` &mdash; Main renderer function that combines named views into a single HTML
  * `onTransition` &mdash; List of default mesasges that should be executed on every router transition
  * `subscriptions` &mdash; A list of subscriptions (see: [elm-lang/html](doc) for details)

  [doc]: http://package.elm-lang.org/packages/apuchenkin/elm-nested-router/latest/URL-Route#Config
-}
type RouterConfig route state msg = RouterConfig {
    html5: Bool
  , removeTrailingSlash: Bool
  , routes: List route
  , routeConfig: route -> RouteConfig route state msg
  , update : Router route state msg -> msg -> state -> (state, Cmd msg)
  , layout: Router route state msg -> state -> Views msg -> Html msg
  , onTransition: Router route state msg -> Maybe (Route route) -> Maybe (Route route) -> List msg
  , subscriptions : state -> Sub msg
  }

{-|
  A `Router` is a provider of following functions:

  * `buildUrl` &mdash; Builds an URL for provided `Route`
  * `forward` &mdash; Preforms a transition to provided `Route`
  * `redirect` &mdash; Redirects to provided `Route`
  * `match` &mdash; Performs attempt to match provided URL.

  Router also exposes it's `config`
-}
type alias Router route state msg = {
    config : RouterConfig route state msg
  , buildUrl : Route route -> URL
  , forward : Route route -> Cmd msg
  , redirect : Route route -> Cmd msg
  , match: URL -> Maybe (Route route)
  }
