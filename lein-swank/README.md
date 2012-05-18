# lein-swank

Leiningen plugin for launching a swank server.

## Usage

From version 1.7.0 on, Leiningen uses a separate list for plugins
rather than `:dev-dependencies`. If you are using Leiningen 1.6 or
earlier, continue adding the main `swank-clojure` entry into your
`:dev-dependencies`.

Add `[lein-swank "1.4.3"]` to `:plugins` in `project.clj`.
Then you should have access to the `swank` and `jack-in` tasks.

## License

Copyright © 2012 Phil Hagelberg

Distributed under the Eclipse Public License, the same as Clojure.
