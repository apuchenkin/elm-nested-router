# Elm nested router

A simple router for single page applications, written in Elm.
Inspired by [angular-ui-router](https://github.com/angular-ui/ui-router) and [Rails router](http://guides.rubyonrails.org/routing.html)

**Elm nested router** allows to separate application to set of handlers, binded to specific routes. For every *Route* you can provide a list of actions that has to be performed on entering that *Route*, and a *view* function that manage to render specific to *Route* HTML parts.

Example: [live demo](http://apuchenkin.github.io/elm-nested-router/example)

### Currently supports
- [x] HTML5 push state
- [x] Route params
- [x] Params constraints (String, Int, Enum, Regexp)
- [x] Optional params
- [x] Named views

### Future thoughts
- [ ] Improve perfomance
- [ ] Better handling for trailing slashes
- [ ] Greedy matcher config (matches first found route or latest one / bypass route config)
- [ ] Check the possibility to replace router cache with Automaton
- [ ] Query string parameters support
- [ ] Drop dependencies on libs
