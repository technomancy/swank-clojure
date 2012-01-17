# Swank Clojure NEWS -- history of user-visible changes

## 1.3.4 / 2011-12-27

* Integrate clj-stacktrace with slime debugger buffers.
* Inspector now supports showing constructors and interfaces on classes.
* Make `clojure-jack-in` more forgiving of boot-time lein output.

## 1.3.3 / 2011-10-04

* Load elisp payloads from various jars during jack-in.
* Add support for \*out\* root value going to repl buffer.
* Check for $PORT as default port.
* Byte-compile elisp source to disk rather than spitting afresh every time.

## 1.3.2 / 2011-07-12

* Cause the Swank server to explicitly block.

## 1.3.1 / 2011-05-16

* Allow for customized announce message.
* Add lein jack-in task.
* Support :repl-init option from project.clj.

## 1.3.0 / 2011-03-22

* Add Clojure 1.3 support.
* M-x slime-load-file (C-c C-l) causes full :reload-all.
* Better support for running on the bootstrap classpath.
* Get encoding from locale.
* Bind to localhost by default rather than 0.0.0.0.
* Include Leiningen shell wrapper for standalone sessions.
* Support completion on class names.

## 1.2.0 / 2010-05-15

* Move lein-swank plugin to be bundled with swank-clojure.
* Support M-x slime-who-calls. List all the callers of a given function.
* Add swank.core/break.
* Support slime-pprint-eval-last-expression.
* Improve support for trunk slime.
* Completion for static Java members.
* Show causes of exceptions in debugger.
* Preserve line numbers when compiling a region/defn.
* Relicense to the EPL (same as Clojure).
* Better compatibility with Clojure 1.2.

## 1.1.0 / 2010-01-01

* Added slime-list-threads, killing threads.
* Dim irrelevant sldb stack frames.
* Emacs 22 support.

## 1.0.0 / 2009-11-10

* First versioned release.
