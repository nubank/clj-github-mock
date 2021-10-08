# clj-github-mock

_current version:_

[![Current Version](https://img.shields.io/clojars/v/dev.nubank/clj-github-mock.svg)](https://clojars.org/dev.nubank/clj-github-mock)

_docs:_
[Found on cljdoc](https://cljdoc.xyz/d/nubank/clj-github-mock/)

`clj-github-mock` provides a `ring` like handler that emulates the github api.
Since most clojure http servers are compatible with the `ring` protocol,
you can use the handler with either `ring` itself or `http-kit` or `pedestal` for example.
Most commonly though you will use the handler combined with a library that fakes http requests
like `clj-http-fake` or `http-kit-fake`. That way you can test that your code that interacts
with the github api is working by using `clj-github-mock` behind the scenes.

**Important**: `clj-github-mock` does not cover the entire github api and should not
be used as the only way to test your github application. Make sure you test your code
against the real github api itself.

## Using state-flow

`clj-github-mock` provides a few [state-flow](https://github.com/nubank/state-flow) helpers that you can use to test your application.

### with-fake-http

Any requests that happen inside `with-fake-http` will be redirected to `clj-github-mock` handler using http-kit fake.

### with-responses

It allows intercepting requests made to `clj-github-mock` and make it either return a fixed response or modify the response.
`with-responses` should be used when `clj-github-mock` does not implement a feature of the API.
It must be called in the context of a `with-fake-http` call.

### gen-ents

It uses specmonstah under the hood to auto generate github entities that can be used in the context of the test.

It must be called in the context of a `with-fake-http` call.

### Anatomy of a state-flow test

## Using plain clojure tests
