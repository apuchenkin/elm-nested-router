module App.Handlers exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Router.Types exposing (Router, Handler, Action)

import App.Routes as Route exposing (Route)
import App.Actions exposing (..)

staticHandler : String -> Router Route State -> Handler State
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

notFoundHandler : Router Route State -> Handler State
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

homeLink : Router Route State -> Html (Action State)
homeLink router =
    Html.a (router.bindForward (Route.Home, Dict.empty) []) [Html.text "Home"]

categoryLink : Router Route State -> Category -> Html (Action State)
categoryLink router category =
  let
    params = [("category", category.id)]
    attributes = []
  in
    Html.a (router.bindForward (Route.Category, Dict.fromList params) attributes) [Html.text category.title]

postLink : Router Route State -> State -> Post -> Html (Action State)
postLink router state post =
  let
    params = Dict.fromList [("postId", toString post.id)]
    attributes = []
  in
    Html.a (router.bindForward (Route.Post, Dict.union params state.router.params) attributes) [Html.text post.title]

-- can be easily lazified
renderCategories : Router Route State -> List Category -> Html (Action State)
renderCategories router categories = Html.div [Attr.class "categories"] [
    Html.h2 [] [Html.text "Categories"],
    Html.ul []
        <| List.map (\category -> Html.li [] [categoryLink router category])
        <| categories
      ]

-- can be easily lazified
renderPosts : Router Route State -> State -> List Post -> Html (Action State)
renderPosts router state posts = Html.div [Attr.class "posts"] [
    Html.h2 [] [Html.text "Posts"],
    Html.ul []
        <| List.map (\post -> Html.li [] [postLink router state post])
        <| posts
      ]

homeHandler : Router Route State -> Handler State
homeHandler router =
  let
    view state _ = Dict.fromList [("body", renderCategories router state.categories)]
  in
    {
      view = view,
      actions = [
        loadCategories router
      ]
    }

categoryHandler : Router Route State -> Handler State
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
        loadPosts router
      ]
    }

postHandler : Router Route State -> Handler State
postHandler router =
  let
    view state _ =
      let
        category = getCategory state
        title = Maybe.withDefault "" <| Maybe.map (.title) state.post
        text =  Maybe.withDefault "" <| state.post &> \post -> post.text
      in Dict.fromList [
        ("header", Html.header [] [homeLink router, Html.text " >> ", Html.text <| Maybe.withDefault "error" <| getCategory state])
      , ("post",  Html.div [Attr.class "post"] [Html.h1 [] [Html.text title], Html.text text])
      ]
  in
    {
      view = view,
      actions = [
        loadPost
      ]
    }
