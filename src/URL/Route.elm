module URL.Route exposing (
    Route, Config, GetConfig,
    route, config, (//>),
    isChild, childs, parents, traverse
  )

{-| Module for defining routes with Segments

# Types
@docs Route, Config, GetConfig

# Constructors
@docs route, config, (//>)

# Traversal utilities
@docs isChild, childs, parents, traverse
-}

import Dict
import URL.Arguments as Arguments exposing (Arguments)
import URL.Segments as Segments exposing (Segment, (</>))

{-| combined abstract route type with params -}
type alias Route route = {
    route: route
  , arguments: Arguments
  , query: Arguments
  }

{-| Route constructor -}
route : route -> Arguments -> Route route
route route arguments = { route = route, arguments = arguments, query = Dict.empty }

{-| Route config -}
type alias Config route = {
    segment: Segment
  , parent: Maybe route
  }

{-| Retreives config for specified route -}
type alias GetConfig route = route -> Config route

{-| Route config constructor -}
config : Maybe route -> Segment -> Config route
config parent segment = {
    segment = segment
  , parent = parent
  }

{-| Infix version of config, combines parent and segment in route config -}
(//>) : Maybe route -> Segment -> Config route
(//>) = config
infixl 6 //>

{-| Checks whether route is a child of another route -}
isChild : GetConfig route -> Maybe route -> route -> Bool
isChild getConfig parent child = parent == (.parent << getConfig) child

{-| Retreives direct childs for a given route -}
childs : GetConfig route -> List route -> Maybe route -> List route
childs getConfig routes route = List.filter (isChild getConfig route) routes

{-| Retreives parents chain of a givent route -}
parents : GetConfig route -> List route -> route -> List route
parents getConfig routes route = case (.parent << getConfig) route of
  Nothing -> []
  Just parent -> parents getConfig routes parent ++ [parent]

{-| Retreives route traversal from top up to a given route -}
traverse : GetConfig route -> List route -> route -> List route
traverse getConfig routes route = parents getConfig routes route ++ [route]
