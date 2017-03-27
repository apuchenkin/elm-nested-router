module App.Handlers exposing (..)

import Json.Decode as Json
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onWithOptions)
import Router.Types as Router exposing (Router)
import URL.Route exposing (route)

import App.Routes as Route exposing (Route)
import App.Actions exposing (..)

type alias Render = Router Route State Msg -> State -> Dict String (Html Msg) -> Dict String (Html Msg)

{-| binds forward action to existing HTML attributes. Exposed by `Router` -}
bindForward : Router Route State Msg -> URL.Route.Route Route -> List (Html.Attribute Msg)
bindForward router route =
  let
    options = {stopPropagation = True, preventDefault = True}
  in [
    Attr.href (router.buildUrl route)
  , onWithOptions "click" options (Json.succeed <| Forward route)
  ]

renderStatic : String -> Render
renderStatic page router state _ =
  let
    body = Html.text page
  in Dict.fromList [
      ("header", Html.header [] [homeLink router, Html.text " >> ", Html.text page])
    , ("body", body)
    ]

notFound : Render
notFound router state _ =
  let
    body = Html.text "404"
  in Dict.fromList [
      ("header", Html.header [] [homeLink router])
    , ("body", body)
    ]

homeLink : Router Route State Msg -> Html Msg
homeLink router =
    Html.a (bindForward router <| route Route.Home Dict.empty) [Html.text "Home"]

categoryLink : Router Route State Msg-> Category -> Html Msg
categoryLink router category =
  let
    params = Dict.fromList [("category", category.id)]
  in
    Html.a (bindForward router <| route Route.Category params) [Html.text category.title]

postLink : Router Route State Msg -> State -> Post -> Html Msg
postLink router state post =
  let
    params = Dict.fromList [("postId", toString post.id)]
  in
    Html.a (bindForward router <| route Route.Post (Dict.union params state.router.arguments)) [Html.text post.title]

-- can be easily lazified
renderCategories : Router Route State Msg -> List Category -> Html Msg
renderCategories router categories = Html.div [Attr.class "categories"] [
    Html.h2 [] [Html.text "Categories"],
    Html.ul []
        <| List.map (\category -> Html.li [] [categoryLink router category])
        <| categories
      ]

-- can be easily lazified
renderPosts : Router Route State Msg -> State -> List Post -> Html Msg
renderPosts router state posts = Html.div [Attr.class "posts"] [
    Html.h2 [] [Html.text "Posts"],
    Html.ul []
        <| List.map (\post -> Html.li [] [postLink router state post])
        <| posts
      ]

renderHome : Render
renderHome router state _ = Dict.fromList [("body", renderCategories router state.categories)]

renderCategory : Render
renderCategory router =
  let
    render state parsed =
    let body = renderPosts router state state.posts
    in Dict.fromList [
      ("header", Html.header [] [homeLink router, Html.text " >> ", Html.text <| Maybe.withDefault "error" <| getCategory state])
    , ("body", Html.div [Attr.class "content"] <| List.filterMap identity [Just body, Dict.get "post" parsed])
    ]
  in render

renderPost : Render
renderPost router state _ =
  let
    category = getCategory state
    title = Maybe.withDefault "" <| Maybe.map (.title) state.post
    text =  Maybe.withDefault "" <| (state.post |> Maybe.andThen (\post -> post.text))
  in Dict.fromList [
    ("header", Html.header [] [homeLink router, Html.text " >> ", Html.text <| Maybe.withDefault "error" <| getCategory state])
  , ("post",  Html.div [Attr.class "post"] [Html.h1 [] [Html.text title], Html.text text])
  ]
