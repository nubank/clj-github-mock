# clj-github-mock

`clj-github-mock` provides a `ring` like handler that emulates the github api.
Since most clojure http servers are compatible with the `ring` protocol,
you can use the handler with either `ring` itself or `http-kit` or `pedestal` for example.
Most commonly though you will use the handler combined with a library that fakes http requests
like `clj-http-fake` or `http-kit-fake`. That way you can test that your code that interacts
with the github api is working by using `clj-github-mock` behind the scenes.

**Important**: `clj-github-mock` does not cover the entire github api and should not
be used as the only way to test your github application. Make sure you test your code
against the real github api itself.

## License

Copyright © 2020 Nubank
