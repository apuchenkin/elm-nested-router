module App.Handlers exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Router.Types exposing (Router, Handler)
import Router.Types as Router

import App.Routes as Route exposing (Route)
import App.Actions exposing (..)

staticHandler : String -> Router Route State Msg -> Handler Route State Msg
staticHandler page router =
  let
    body = Html.text page
    view state parsed = Dict.fromList [
      ("header", Html.header [] [homeLink router, Html.text " >> ", Html.text page])
    , ("body", body)
    ]
  in
    {
      view = view,
      actions = []
    }

notFoundHandler : Router Route State Msg -> Handler Route State Msg
notFoundHandler router =
  let
    body = Html.text "404"
    view state _ = Dict.fromList [
      ("header", Html.header [] [homeLink router])
    , ("body", body)
    ]
  in
    {
      view = view,
      actions = []
    }

homeLink : Router Route State Msg -> Html (Router.Msg Route Msg)
homeLink router =
    Html.a (router.bindForward (Route.Home, Dict.empty) []) [Html.text "Home"]

categoryLink : Router Route State Msg-> Category -> Html (Router.Msg Route Msg)
categoryLink router category =
  let
    params = [("category", category.id)]
    attributes = []
  in
    Html.a (router.bindForward (Route.Category, Dict.fromList params) attributes) [Html.text category.title]

postLink : Router Route State Msg -> State -> Post -> Html (Router.Msg Route Msg)
postLink router state post =
  let
    params = Dict.fromList [("postId", toString post.id)]
    attributes = []
  in
    Html.a (router.bindForward (Route.Post, Dict.union params state.router.params) attributes) [Html.text post.title]

-- can be easily lazified
renderCategories : Router Route State Msg -> List Category -> Html (Router.Msg Route Msg)
renderCategories router categories = Html.div [Attr.class "categories"] [
    Html.h2 [] [Html.text "Categories"],
    Html.ul []
        <| List.map (\category -> Html.li [] [categoryLink router category])
        <| categories
      ]

-- can be easily lazified
renderPosts : Router Route State Msg -> State -> List Post -> Html (Router.Msg Route Msg)
renderPosts router state posts = Html.div [Attr.class "posts"] [
    Html.h2 [] [Html.text "Posts"],
    Html.ul []
        <| List.map (\post -> Html.li [] [postLink router state post])
        <| posts
      ]

homeHandler : Router Route State Msg -> Handler Route State Msg
homeHandler router =
  let
    view state _ = Dict.fromList [("body", renderCategories router state.categories)]
  in
    {
      view = view,
      actions = [
        LoadCategories
      ]
    }

categoryHandler : Router Route State Msg -> Handler Route State Msg
categoryHandler router =
  let
    view state parsed =
    let body = renderPosts router state state.posts
    in Dict.fromList [
      ("header", Html.header [] [homeLink router, Html.text " >> ", Html.text <| Maybe.withDefault "error" <| getCategory state])
    , ("body", Html.div [Attr.class "content"] <| List.filterMap identity [Just body, Dict.get "post" parsed])
    ]
  in
    {
      view = view,
      actions = [
        LoadPosts
      ]
    }

postHandler : Router Route State Msg -> Handler Route State Msg
postHandler router =
  let
    view state _ =
      let
        category = getCategory state
        title = Maybe.withDefault "" <| Maybe.map (.title) state.post
        text =  Maybe.withDefault "" <| (state.post |> Maybe.andThen (\post -> post.text))
      in Dict.fromList [
        ("header", Html.header [] [homeLink router, Html.text " >> ", Html.text <| Maybe.withDefault "error" <| getCategory state])
      , ("post",  Html.div [Attr.class "post"] [Html.h1 [] [Html.text title], Html.text text])
      ]
  in
    {
      view = view,
      actions = [
        LoadPost
      ]
    }
