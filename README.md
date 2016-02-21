# Elm nested router

A simple router for single page applications, written in Elm

before release
- update documentation
- complete/fix tests
- private / public contexts types

### Currently supports
- [x] HTML5 push state
- [x] Route params
- [x] Params constraints (String, Int, Enum, Regexp)
- [x] Optional params

### Todos
- [ ] Add example
- [ ] Improve perfomance
- [ ] greedy matcher config (matches first found route or latest one)
- [ ] Better handling for trailing slashes
- [ ] Support for named views (Maybe Html -> Dict Html)
