module Router.Types where
{-| Router types

# URL parts
@docs URL, RawURL, RawSegment, Param, Constraint, Route, RouteConfig, RouteParams, Matcher

# Actions and handlers
@docs WithRouter, Handler, Action, ActionEffects, Response, Transition

# Router
@docs Router, RouterConfig, RouterResult, RouterState
-}

import Dict           exposing (Dict)
import Html           exposing (Html)
import Task           exposing (Task)
import Effects        exposing (Effects, Never)
import MultiwayTree   exposing (Tree, Forest)

-----------------------------------------
-- Route mather related types
-----------------------------------------

{-| A valid URL:
```
"/home/post/1"
```
-}
type alias URL = String

{-| Raw URL template:
```
"/home/post/:postId"
```
-}
type alias RawURL = String

{-| A single segment of `RawURL` template -}
type alias RawSegment = String

{-| Dynamic route parameter name -}
type alias Param = String

{-| A map of route param names and values -}
type alias RouteParams  = Dict Param String

{-| A constraint of route parameter type -}
type Constraint = Int | String | Enum (List String) | Regex String

{-| combined abstract route type with params -}
type alias Route route = (route, RouteParams)

-----------------------------------------
-- Handler related types
-----------------------------------------

{-| An action result - a modified state combined with side effects -}
type Response state = Response (state, ActionEffects state)

{-| `Action` represents function that prforms something with application state, and might contain side efects -}
type alias Action state = state -> Response state

{-| Helper to get rid of brackets -}
type alias ActionEffects state = Effects (Action state)

{-|
  A `Handler` is a piece of functionality binded to specific route
  * `view` &mdash; Function that describes how to render application state to map of named views
  * `actions` &mdash; A set of necessary to perform actions
-}
type alias Handler state = {
    view: state -> Dict String Html -> Dict String Html
  , actions: List (Action state)
  }

{-|
  `RouteConfig` is a route configuration

  * `segment` &mdash; URL segment

  Expample:
```
"/home",
"/post/:postId",
"/author[/:authorId]"
```
  * `constraints` &mdash; A set of constraints applied to route params (`String`, `Int`, `Enum`, `Regexp`)
  * `handler` &mdash; A binding to handler. Router might be injected in handler

  **Exapmle of route configuration**:
```
  config = {
    -- "author" and "postId" is dynamic url parts
    -- "postId" is marked as optional and might me ommited in URL
    segment = "/page/:author[/:postId]"
    -- constraints specify that `author` param must be a string,
    -- and postId an integer
  , constraints = Dict.fromList [("author", String),("postId", Int)]
  , handler = always PostHandler
  }
```

  A `config` above will match following URLs:
```
"/page/mark/1", "/page/mark", "/page/joe"
```
  "mark" and "joe" will be stored as `author` param, and "1" as `postId`
  Everything enclosed by brackets considered as optional.
-}
type alias RouteConfig state = {
    segment: RawSegment
  , constraints: Dict Param Constraint
  , handler: Handler state
  }

-- {-| Router cache -}
-- type alias RouterCache route = {
--     rawUrl: Dict String RawURL
--   , unwrap: Dict String (List String)
--   , traverse: Dict String (List route)
--   }

{-| A state of router -}
type alias RouterState route = {
    route: Maybe route
  , params: RouteParams
  }

{-| Type extension for the application state -}
type alias WithRouter route state = { state | router : RouterState route}

{-| A transition from route A to route B -}
type alias Transition route state = Maybe (Route route) -> Route route -> Action state

{-|
  `RouterConfig` is configuration for the router:

  * `init` &mdash; Initial application state
  * `useCache` &mdash; A boolean flag that turns caching on or off. Using cache might slow down application at start-up but will give a perfomance boost in runtime.
  * `html5` &mdash; Use html5 pushState
  * `removeTrailingSlash` &mdash; Trailing slashes will be removed from matched and builded urls
  * `fallback` &mdash; A fallback route is used when url matching fails
  * `layout` &mdash; Main rendered function that combines named views gathered from Handlers in a single HTML
  * `onTransition` &mdash; An action that should be executed on every router transition
  * `routeConfig` &mdash; A mapping between route and route configuration
  * `routes` &mdash; A list of route trees, used for nested navigation
  * `inits` &mdash; A list of signals that should run for inititialisation of state
  * `inputs` &mdash; A list of signals utilized in application in runtime
-}
type RouterConfig route state = RouterConfig {
    init: state
  -- , useCache: Bool
  , html5: Bool
  , removeTrailingSlash: Bool
  , fallback: Route route
  , layout: Router route state -> state -> Dict String Html -> Html
  , onTransition: Router route state -> Transition route state
  , routeConfig: route -> RouteConfig state
  , routes: Forest route
  , inits: List (Signal.Signal (Action state))
  , inputs: List (Signal.Signal (Action state))
  }

{-| A `Matcher` is a provider of following functions:
  * `unwrap` &mdash; TODO: write description
    This is useful to create links in application
  * `composeRawUrl` &mdash; TODO: write description
  * `getPath` &mdash; TODO: write description
-}
type alias Matcher route state = {
    unwrap: String -> List String
  , composeRawUrl: route -> RawURL
  , getPath: route -> List route
  , getConfig: route -> RouteConfig state
  }

{-| A `Router` is a provider of following functions:
  * `bindForward` &mdash; Binds a `forward` action to a provided `Route` with a list of html attributes.
    This is useful to create links in application
  * `buildUrl` &mdash; Builds an URL for provided `Route`
  * `forward` &mdash; Preforms a transition to provided `Route`
  * `redirect` &mdash; Redirects to provided `Route`
-}
type alias Router route state = {
    config : RouterConfig route state
  , address: Signal.Address (Action state)
  , matcher: Matcher route state
  , bindForward : Route route -> List Html.Attribute -> List Html.Attribute
  , buildUrl : Route route -> URL
  , forward : Route route -> Action state
  , redirect : Route route -> Action state
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
  { html : Signal Html
  , state : Signal state
  , tasks : Signal (Task Never ())
  }
