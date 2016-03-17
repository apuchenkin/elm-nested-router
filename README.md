# Elm nested router [![Build Status](https://travis-ci.org/apuchenkin/elm-nested-router.svg?branch=master)](https://travis-ci.org/apuchenkin/elm-nested-router)

A simple router for single page applications, written in Elm.
Inspired by [angular-ui-router](https://github.com/angular-ui/ui-router) and [Rails router](http://guides.rubyonrails.org/routing.html)

**Elm nested router** allows to separate application logic to handlers binded to specific routes. For every `Route` you can provide a list of actions that has to be performed on entering to `Route`, and a `view` function that renders `Route`  specific HTML parts.

## Example

To create a routeable application we have to keep `Router` state (Current route and params) in application state.
In order to do this we create our application state with a helper `WithRouter`:

```elm
-- We use `WithRouter` to define application state
type alias State = WithRouter Route
  {
    categories: List Category,
    posts: List Post,
    post: Maybe Post
  }

-- construct initialState with blanks
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
routes : Forest Route
routes = [
    NotFound
  , Static "about"  
  , Static "contacts"
  , Home
  , Category
  , Post
  ]
```
Route `Post` on the example above is rely on routes `Category` and `Home` - that means all actions binded to routes `Home`, `Category` and `Post` will be executed to enter to `Post` route. Full URL template that will match `Post` route will also be combined with its parent routes.

`Category` route is concurrent to `Static "about"` ("\\about" - can match both `Category` and `Static` routes) so `Static "about"` will be matched there since it's declared first. Otherwise `Category` with a param "about" will be matched

Another thing that we need to define is a mapping between routes and route configurations:
```elm
routeConfig : Route -> RouteConfig Route State
routeConfig route = case route of
  Home -> {
      segment = "/",
      parent = Nothing,
      bypass = False,
      constraints = Dict.empty,
      handler = homeHandler
    }
  NotFound -> {
      segment = "/404",
      parent = Nothing,
      bypass = False,
      constraints = Dict.empty,
      handler = notFoundHandler
    }
  Static page -> {
      segment = "/" ++ page,
      parent = Nothing,
      bypass = False,
      constraints = Dict.empty,
      handler = staticHandler page
    }
  Category -> {
      -- `:category` and `:subcategory` is dynamic route params
      -- `:category` param match only "animals", "flowers", "colors" because of its constraints
      -- `:subcategory` might be omitted, since it enclosed brackets
      segment = ":category[/:subcategory]",
      parent = Just Home,
      bypass = False,
      constraints = Dict.fromList [("category", Enum ["animals", "flowers", "colors"])],
      handler = categoryHandler
    }
  Post -> {
      -- `:postId` must be an integer
      segment = "/post/:postId",
      parent = Just Category,
      bypass = False,
      constraints = Dict.fromList [("postId", Int)],
      handler = postHandler
    }
```

Each handler provides named views: `Dict String Html` - these HTML parts are finally combined and rendered in application layout:

```elm
layout : Router Route State -> State -> Dict String Html -> Html
layout router _ views =
  let
    defaultHeader = Html.header [] [Html.text "Default header"]
    defaultFooter = Html.footer [] [Html.text "Default footer"]
    defaultBody = Html.div [] []
  in Html.div [] [
    Maybe.withDefault defaultHeader <| Dict.get "header" views
  , Maybe.withDefault defaultBody   <| Dict.get "body"   views
  , Maybe.withDefault defaultFooter <| Dict.get "footer" views
  ]
```

Now we have everything needed to create a router:
```elm
router : Router Route State
router = Router.router <| RouterConfig {
    init = initialState
  , html5 = True
  , removeTrailingSlash = True
  , fallback = (NotFound, Dict.empty)
  , layout = layout
  , onTransition = \_ _ _ -> doNothing
  , routes = routes
  , routeConfig = routeConfig
  , inits = []
  , inputs = []
  }
```

Finally we launch Router and feed its output to application ports:

```elm
result : RouterResult State
result = Router.runRouter router

main : Signal Html
main = result.html

port tasks : Signal (Task Never ())
port tasks = result.tasks
```

see [Example](https://github.com/apuchenkin/elm-nested-router/tree/master/example) and [Tests](https://github.com/apuchenkin/elm-nested-router/tree/master/test/Test) for more details ([Live demo](http://apuchenkin.github.io/elm-nested-router/example))

### Currently supports
- [x] HTML5 push state
- [x] Route params
- [x] Params constraints (String, Int, Enum, Regexp)
- [x] Optional params
- [x] Named views

### Future thoughts
- [x] Drop dependencies on libs
- [x] Better handling for trailing slashes
- [x] Greedy matcher config (matches first found route or latest one / bypass route config)
- [x] Check the possibility to replace router cache with Automaton
- [ ] Improve perfomance on specific functions
- [ ] Query string parameters support
