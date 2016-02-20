# Elm nested router

A simple router for single page applications, written in Elm

before release
- update documentation
- complete/fix tests
- private / public contexts types
- drop unneeded deps on libs
- (Bug) replacePath instead setPath on fallback (404)

### Currently supports
- [ ] HTML5 push state
- [ ] Route params
- [ ] Params constraints (String, Int, Enum, Regexp)
- [ ] Optional params

### Todos
- Add example
- Improve perfomance
- greedy matcher config (matches first found route or latest one)
- Better handling for trailing slashes
- Support for named views (Maybe Html -> Dict Html)
