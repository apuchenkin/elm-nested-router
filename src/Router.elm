module Router exposing (dispatch, initialState)

{-| A simple nested router for single page applications.

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) ([Live demo](http://apuchenkin.github.io/elm-nested-router/example))
and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details

@docs dispatch, initialState
-}

import Dict
import Navigation exposing (Location)
import Router.Matcher as Matcher exposing (Matcher)
import Router.Types exposing (..)
import Router.Functions exposing (..)
import Router.Navigation exposing (..)


{-| Initial state for router. Fed this into your application state
-}
initialState : RouterState route
initialState =
    { route = Nothing
    , params = Dict.empty
    }


{-| Router constructor
-}
constructor : RouterConfig route state -> Matcher route state -> Router route state
constructor config matcher =
    let
        (RouterConfig c) =
            config

        config_ =
            RouterConfig <| { c | routeConfig = matcher.getConfig }
    in
        { config = config_
        , bindForward = bindForward config_ matcher
        , buildUrl = buildUrl config_ matcher
        , forward = forward config_ matcher
        , redirect = redirect config_ matcher
        , match = matchRoute matcher
        }


{-| Launches the router.
  Provide `init` function and router config as parameters
-}
dispatch : (flags -> ( WithRouter route state, Cmd (Action (WithRouter route state)) )) -> RouterConfig route (WithRouter route state) -> Program flags



-- flags


dispatch init config =
    let
        (RouterConfig c) =
            config

        matcher =
            Matcher.matcher config

        router =
            constructor config matcher

        getHandlers =
            createHandlers router matcher

        render_ =
            render router <| List.map getHandlers << matcher.traverse

        urlUpdate route =
            runAction <| transition router matcher getHandlers route

        parser =
            Navigation.makeParser <| matcher.match << getPath config

        init_ flags route =
            let
                ( state, cmd ) =
                    init flags

                ( state_, cmd_ ) =
                    urlUpdate route state
            in
                ( state_, Cmd.batch [ cmd, cmd_ ] )
    in
        Navigation.programWithFlags parser
            { init = init_
            , update = runAction
            , urlUpdate = urlUpdate
            , view = render_
            , subscriptions = c.subscriptions
            }
