# Elm nested router [![Build Status](https://travis-ci.org/apuchenkin/elm-nested-router.svg?branch=master)](https://travis-ci.org/apuchenkin/elm-nested-router)

A simple router for single page applications, written in Elm.

**Elm nested router** allows to separate application logic by specific routes.
For every `Route` you can provide a list of messages that has to be performed on entering to `Route`, and a `render` function that renders `Route` specific HTML parts.

## Example

To create a routeable application we have to keep `Router` state (Current route and arguments) in application state.
In order to do this we create our application state by using a helper `WithRouter`:

```elm
import Router
import Router.Types exposing (WithRouter)

-- We use `WithRouter` to define application state
type alias State = WithRouter Route
  {
    categories: List Category,
    posts: List Post,
    post: Maybe Post
  }

-- construct initialState with initial values
initialState : State
initialState = {
    router      = Router.initialState
  , categories  = []
  , posts       = []
  , post        = Nothing
  }
```

Next step will be the definition of application routes:
```elm
type Route = Home | NotFound | Static String | Category | Post

-- Route list that is required by router.
-- Note that routes which registered first have higher priority to be matched. So when you have concurrent routes, order of this list is important.
routes : List Route
routes = [
    NotFound
  , Static "about"
  , Static "contacts"
  , Home
  , Category
  , Post
  ]
```

Another thing that we need to define is a mapping between routes and route configurations:

```elm
import Router.Types exposing (RouteConfig)
import URL.Segments as Segments exposing ((</>))
import URL.Route exposing ((//>))

routeConfig : Route -> RouteConfig Route State
routeConfig route = case route of
  Home -> {
    route = Nothing //> Segments.end,
  , render = renderHome
  , actions = [LoadCategories]
  }
  NotFound -> {
    route = Nothing //> Segments.static "404",
  , render = notFound
  , actions = []
  }
  Static page -> {
    route = Nothing //> Segments.static page
  , render = renderStatic page
  , actions = []
  }
  -- `category` and `subcategory` is dynamic route params
  -- `category` param match only "animals", "flowers", "colors" because of its constraints
  -- `subcategory` is optional and might be omitted
  Category -> {
    route = Just Home //> Segments.enum "category" ["animals", "flowers", "colors"] </> Segments.maybe (Segments.string "subcategory")
  , render = renderCategory
  , actions = [LoadPosts]
  }
  -- `postId` argument must be an integer
  Post -> {
    route = Just Route.Category //> Segments.static "post" </> Segments.int "postId"
  , render = renderPost
  , actions = [LoadPost]
  }
```

Route `Post` on the example above is rely on routes `Category` and `Home` - that means all actions binded to routes `Home`, `Category` and `Post` will be executed to enter to `Post` route. Full URL template that will match `Post` route will also be combined with its parent routes.

Each handler provides named views: `Dict String Html` - these HTML parts are finally combined and rendered in application layout:

```elm
layout : Router Route State -> State -> Dict String (Html (Action State)) -> Html (Action State)
layout router _ views =
  let
    defaultHeader = Html.header [] [Html.text "Default header"]
    defaultFooter = Html.footer [] [Html.text "Default footer"]
    defaultBody = Html.div [] []
  in Html.div [] [
    Maybe.withDefault defaultHeader <| Dict.get "header" views
  , Maybe.withDefault defaultBody   <| Dict.get "body" views
  , Maybe.withDefault defaultFooter <| Dict.get "footer" views
  ]
```

Finally we could dispacth our application:

```elm
import App.Actions exposing (update)

main = Router.dispatch
  (noFx initialState)
  (RouterConfig {
    html5 = False
  , removeTrailingSlash = True
  , update = update
  , onTransition = []
  , layout = layout
  , routes = routes
  , routeConfig = routeConfig
  , subscriptions = always Sub.none
  })
```

See [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) ([Live demo](http://apuchenkin.github.io/elm-nested-router/example)) and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details.

[Advanced example](https://github.com/apuchenkin/aws.photo.service/tree/elm/client)

### Currently supports
-   [x] HTML5 push state
-   [x] Route params
-   [x] Params constraints (String, Int, Enum, Regexp)
-   [x] Optional params
-   [x] Named views

### Future thoughts
-   [ ] Query string parameters support
