module Router.Types where

import Dict           exposing (Dict)
import Html           exposing (Html)
import Task           exposing (Task)
import Effects        exposing (Effects, Never)
import MultiwayTree   exposing (Tree, Forest)

-----------------------------------------
-- Route mather related types
-----------------------------------------

{-| A valid URL -}
type alias URL    = String

{-| Raw URL template -}
type alias RawURL = String

{-| Single segment of URL template -}
type alias RawSegment = String

{-| Dynamic route parameter name -}
type alias Param   = String

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

{-| Some action with state -}
type alias Action state = state -> Response state

{-| helper to get rid of brackets -}
type alias ActionEffects state = Effects (Action state)

{-| A piece of functionality related to specific route

    view: Fucntion to render state
      `address` - an address to send messages during render
      `state` - application state
      `html`  - Html from rendered handlers
    actions: A set of necessary to perform actions
-}
type alias Handler state = {
    view      : Signal.Address (Action state) -> state -> Maybe Html -> Maybe Html
  , actions   : List (Action state)
  }

{-| Route configuration -}
type alias RouteConfig route state = {
    segment:      RawSegment
  , constraints:  Dict Param Constraint
  , handler:      Router route state -> Handler state
  }

-----------------------------------------
-- Router related types
-----------------------------------------

{-| Type extension for the model. -}
type alias WithRouter route state = { state | router : RouterState route}

type alias Transition route state = Maybe (Route route) -> Route route -> Action state

type alias RouterResult state =
  { html  : Signal Html
  , state : Signal state
  , tasks : Signal (Task Never ())
  }

type alias RouterCache route = {
    rawUrl:     Dict String RawURL
  , unwrap:     Dict String (List String)
  , traverse:   Dict String (List route)
  }

type alias RouterState route = {
    route:  Maybe route
  , params: RouteParams
  , cache:  RouterCache route
  }

type alias RouterConfig route state = {
    init:         state
  , useCache:     Bool
  , fallback:     Route route
  , fallbackHtml: Html
  , config:       route -> RouteConfig route state
  , routes:       Forest route
  , inits:        List (Signal.Signal (Action state))
  , inputs:       List (Signal.Signal (Action state))
  }

type Router route state = Router {
    config        : RouterConfig route state
  , bindForward   : Route route -> List Html.Attribute -> List Html.Attribute
  , buildUrl      : Route route -> URL
  , forward       : Route route -> Action state
  , redirect      : Route route -> Action state
  }
