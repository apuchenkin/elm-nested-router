module Router.Types where
{-| Router types

# URL parts
@docs URL, RawURL, RawSegment, Param, Constraint, Route, RouteConfig, RouteParams

# Actions and handlers
@docs WithRouter, Handler, Action, ActionEffects, Response

# Router
@docs Router, RouterConfig, RouterResult, RouterCache, RouterState
-}

import Dict           exposing (Dict)
import Html           exposing (Html)
import Task           exposing (Task)
import Effects        exposing (Effects, Never)
import MultiwayTree   exposing (Tree, Forest)

-----------------------------------------
-- Route mather related types
-----------------------------------------

{-| A valid URL -}
type alias URL = String

{-| Raw URL template -}
type alias RawURL = String

{-| Single segment of URL template -}
type alias RawSegment = String

{-| Dynamic route parameter name -}
type alias Param = String

{-| A set of route params and their values -}
type alias RouteParams  = Dict Param String

{-| A constraint of route parameter type -}
type Constraint = Int | String | Enum (List String) | Regex String

{-| combined abstract route type with params -}
type alias Route route = (route, RouteParams)

-----------------------------------------
-- Handler related types
-----------------------------------------

{-| An action result - a state combined with effects -}
type Response state = Response (state, ActionEffects state)

{-| `Action` represents function that prforms something with application state, and might contain efects -}
type alias Action state = state -> Response state

{-| Helper to get rid of brackets -}
type alias ActionEffects state = Effects (Action state)

{-|
  A `Handler` is a piece of functionality binded to specific route

  * `view` &mdash; A function that describes how to render application state
  * `actions` &mdash; A set of necessary to perform actions
-}
type alias Handler state = {
    view: Signal.Address (Action state) -> state -> Dict String Html -> Dict String Html
  , actions: List (Action state)
  }

{-|
  `RouteConfig` is a route configuration

  * `segment` &mdash; URL segment
  Expample: "/home", "/post/:postId", "/author[/:authorId]"
  * `constraints` &mdash; A set of constraints applied to route params
  Supported constraints is (String, Int, Enum, Regexp)
  * `handler` &mdash; A binding to handler. Router might be injected in handler

  segment supports following notation:
  - dynamic part of URL is a url param represented by ":" followed by parameter name
  - a piece of URL in brackets represents optional string that might be in URL or might be ommitted

  Exapmle of route configuration:

    config = {
      segment = "/page/:author[/:postId]"
    , constraints = Dict.fromList [("author", String),("postId", Int)]
    , handler = always PostHandler
    }

    where `author` is a dynamic part of URL that matches any string
    where `postId` is a dynamic part of URL that matches any integers and might be ommitted
-}
type alias RouteConfig route state = {
    segment:      RawSegment
  , constraints:  Dict Param Constraint
  , handler:      Router route state -> Handler state
  }

{-| Router cache -}
type alias RouterCache route = {
    rawUrl:     Dict String RawURL
  , unwrap:     Dict String (List String)
  , traverse:   Dict String (List route)
  }

{-| A state of router -}
type alias RouterState route = {
    route:  Maybe route
  , params: RouteParams
  , cache:  RouterCache route
  }

{-| Type extension for the application state -}
type alias WithRouter route state = { state | router : RouterState route}

{-|
  `RouterConfig` is configuration for the router:

  * `init` &mdash; Initial application state
  * `useCache` &mdash; A boolean flag that turns caching on or off. Using cache might slow down application at start-up but will give a perfomance boost in runtime.
  * `html5` &mdash; Use html5 pushState
  * `fallback` &mdash; A fallback route is used when url matching fails
  * `fallbackHtml` &mdash; Default Html, udes when handlres has provided empty view
  * `config` &mdash; A mapping between route and route configuration
  * `routes` &mdash; A list of route trees, used for nested navigation
  * `inits` &mdash; A list of signals that should run for inititialisation of state
  * `inputs` &mdash; A list of signals utilized in application in runtime
-}
type alias RouterConfig route state = {
    init:         state
  , useCache:     Bool
  , html5:        Bool
  , fallback:     Route route
  , layout:       Router route state -> state -> Dict String Html -> Html
  , config:       route -> RouteConfig route state
  , routes:       Forest route
  , inits:        List (Signal.Signal (Action state))
  , inputs:       List (Signal.Signal (Action state))
  }

{-| A `Router` is a provider of following functions:
  * `bindForward` &mdash; Binds a `forward` action to a provided `Route` with a list of html attributes.
    This is usefull to create links in application
  * `buildUrl` &mdash; Buils an URL for provided `Route`
  * `forward` &mdash; Preforms a transition to provided `Route`
  * `redirect` &mdash; Redirects to provided `Route`

  `config` is a router configuration used on router construction
-}
type Router route state = Router {
    config        : RouterConfig route state
  , bindForward   : Route route -> List Html.Attribute -> List Html.Attribute
  , buildUrl      : Route route -> URL
  , forward       : Route route -> Action state
  , redirect      : Route route -> Action state
  }

{-| A `RouterResult` is a combination of resulting signals:
  * `html` &mdash; a signal of `Html` representing the current visual
    representation of your app. This should be fed into `main`.
  * `state` &mdash; a signal representing the central state of your application.
  * `tasks` &mdash; a signal of tasks that need to get run. Your app is going
    to be producing tasks in response to all sorts of events, so this needs to
    be hooked up to a `port` to ensure they get run.
-}
type alias RouterResult state =
  { html  : Signal Html
  , state : Signal state
  , tasks : Signal (Task Never ())
  }
