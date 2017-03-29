module App.Actions exposing (..)

import Http
import Dict exposing (Dict)
import Task exposing (Task)

import Json.Decode  as Json exposing (field)
import Router.Types exposing (WithRouter, Router)
import Router.Types as Router
import URL.Route as Route

import App.Routes as Routes exposing (Route)

type alias State = WithRouter Route {
  categories: List Category,
  posts: List Post,
  post: Maybe Post
}

type alias Category = {
  id: String,
  title: String
}

type alias Post = {
  id: Int,
  title: String,
  text: Maybe String
}

type Msg =
    Forward (Route.Route Route)
  | LoadCategories
  | LoadPosts
  | LoadPost
  | UpdateCategories (List Category)
  | UpdatePosts (List Post)
  | UpdatePost (Maybe Post)

type alias Action = State -> (State, Cmd Msg)

{-| An action without side effects -}
noFx : Action
noFx state = (state, Cmd.none)

getCategory : State -> Maybe String
getCategory state =
  let
    param = case Dict.get "subcategory" state.router.arguments of
      Nothing -> Dict.get "category"    state.router.arguments
      category -> category
  in param

decodeCategories : Json.Decoder (List Category)
decodeCategories = Json.list <| Json.map2 Category
  (field "id"    Json.string)
  (field "title" Json.string)

decodePost : Json.Decoder Post
decodePost = Json.map3 Post
  (field "id"     Json.int)
  (field "title"  Json.string)
  (Json.maybe <| field "text" Json.string)

decodePosts : Json.Decoder (List Post)
decodePosts = Json.list decodePost

loadCategories : Action
loadCategories state =
  let
    fetch = Task.onError (\_ -> Task.succeed []) <| Http.toTask <| Http.get "data/categories.json" decodeCategories
  in (state, Task.perform UpdateCategories fetch)

loadPosts : Action
loadPosts state =
  let
    fetchTask = flip Maybe.map (getCategory state)
      <| \category -> Task.onError (\_ -> Task.succeed []) <| Http.toTask <| Http.get ("data/category/" ++ category ++ ".json") decodePosts

  in (state, Maybe.withDefault Cmd.none <| Maybe.map (Task.perform UpdatePosts) fetchTask)

loadPost : Action
loadPost state =
  let
    postId = Dict.get "postId" state.router.arguments
    task = flip Maybe.map postId <| \pid ->
      Task.onError (\_ -> Task.succeed Nothing) <| Http.toTask <| Http.get ("data/post/" ++ pid ++ ".json") (Json.maybe <| decodePost)

  in (state, Maybe.withDefault Cmd.none <| Maybe.map (Task.perform UpdatePost) task)

updatePosts : List Post -> Action
updatePosts posts state = noFx {state | posts = posts}

updatePost : Maybe Post -> Action
updatePost post state = noFx {state | post = post}

updateCategories : List Category -> Action
updateCategories categories state = noFx {state | categories = categories}

update : Router Route State Msg -> Msg -> Action
update router msg = case msg of
  Forward location -> \state -> (state, router.forward location)
  LoadCategories -> loadCategories
  LoadPosts -> loadPosts
  LoadPost -> loadPost
  UpdateCategories categories  -> updateCategories categories
  UpdatePosts posts -> updatePosts posts
  UpdatePost post -> updatePost post
