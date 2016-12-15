module Test.Mock.Data exposing (..)

import Html exposing (Html)
import Dict exposing (Dict)
import Router.Helpers exposing (noFx, doNothing)
import Router.Types exposing (..)
import Router.Matcher as Matcher exposing (Matcher)
import Router.Functions exposing (createHandlers)
import Test.Mock.Router exposing (..)


type Route
    = Home
    | Page
    | Subpage
    | NotFound


routes : List Route
routes =
    [ NotFound, Home, Page, Subpage ]


type alias State =
    WithRouter Route
        { str : String
        , sum : Int
        }


config : Route -> RouteConfig Route State
config route =
    case route of
        Home ->
            { segment = "/"
            , bypass = False
            , parent = Nothing
            , constraints = Dict.empty
            , handler = always handlerA
            }

        NotFound ->
            { segment = "/404"
            , bypass = False
            , parent = Nothing
            , constraints = Dict.empty
            , handler = always handlerA
            }

        Page ->
            { segment = ":category[/:subcategory]"
            , bypass = False
            , parent = Just Home
            , constraints = Dict.fromList [ ( "category", Enum [ "A", "B", "C" ] ) ]
            , handler = always handlerB
            }

        Subpage ->
            { segment = "/item/:item"
            , bypass = False
            , parent = Just Page
            , constraints = Dict.fromList [ ( "item", Int ) ]
            , handler = always handlerC
            }


init : State
init =
    { router = initialState
    , str = ""
    , sum = 0
    }


layout : Router Route State -> State -> Dict String (Html (Action State)) -> Html (Action State)
layout _ _ parsed =
    let
        fallback =
            Html.text "error"
    in
        Maybe.withDefault
            (Maybe.withDefault
                (Maybe.withDefault fallback <|
                    Dict.get "handlerA" parsed
                )
             <|
                Dict.get "handlerB" parsed
            )
        <|
            Dict.get "handlerC" parsed


routerConfig : RouterConfig Route State
routerConfig =
    RouterConfig
        { html5 = True
        , removeTrailingSlash = True
        , layout = layout
        , transition = \_ _ _ -> doNothing
        , routes = routes
        , routeConfig = config
        , subscriptions = always Sub.none
        }



------------ actions ----------------------


noAction : Action State
noAction state =
    Response <| noFx state


succ : Action State
succ state =
    Response <| noFx { state | sum = state.sum + 1 }


append : String -> Action State
append string state =
    Response <| noFx { state | str = state.str ++ string }



------------ handlers ----------------------
-- only text html can be tested due to bug (toString(null))


handlerA : Handler State
handlerA =
    { view = \state _ -> Dict.fromList [ ( "handlerA", Html.text "handlerA" ) ]
    , actions =
        [ noAction
        ]
    }


handlerB : Handler State
handlerB =
    { view = \state _ -> Dict.fromList [ ( "handlerB", Html.text <| toString state.sum ) ]
    , actions =
        [ succ
        ]
    }


handlerC : Handler State
handlerC =
    { view = \state _ -> Dict.fromList [ ( "handlerC", Html.text state.str ) ]
    , actions =
        [ succ
        , append "foo"
        ]
    }


router : Router Route State
router =
    routerMock routerConfig


matcher : Matcher Route State
matcher =
    Matcher.matcher routerConfig


getHandlers : Route -> Handler (WithRouter Route { str : String, sum : Int })
getHandlers =
    createHandlers router matcher
