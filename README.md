# Swank Clojure

[Swank Clojure](http://github.com/technomancy/swank-clojure) is a
server that allows SLIME (the Superior Lisp Interaction Mode for
Emacs) to connect to Clojure projects.

## Deprecated

This project is no longer under active development.

New users are strongly encouraged to try out
[nrepl.el](https://github.com/kingtim/nrepl.el) instead. If you need
an advanced debugger, [Ritz](https://github.com/pallet/ritz) might be
a better fit.

## Usage

The simplest way is to just "jack in" from an existing project
using [Leiningen](http://github.com/technomancy/leiningen):

* Install `clojure-mode` either from
  [Marmalade](http://marmalade-repo.org) or from
  [git](http://github.com/technomancy/clojure-mode).
* Add `[lein-swank "1.4.5"]` to the `:plugins` section of either
  `project.clj` or your user profile.
* From an Emacs buffer inside a project, invoke `M-x clojure-jack-in`

If you are still using a version of Leiningen older than 1.7.0, you
will need to do `lein plugin install swank-clojure 1.4.3` instead.

That's all it takes; there are no extra install steps beyond
`clojure-mode` on the Emacs side and the `swank-clojure` plugin on the
Leiningen side. In particular, be sure you **don't have any other
versions of SLIME loaded**; see "Troubleshooting" below.

## SLIME Commands

Commonly-used SLIME commands:

* **M-.**: Jump to the definition of a var
* **M-TAB** or **C-c TAB**: Autocomplete symbol at point
* **C-x C-e**: Eval the form under the point
* **C-c C-k**: Compile the current buffer
* **C-c C-l**: Load current buffer and force required namespaces to reload
* **C-M-x**: Compile the whole top-level form under the point.
* **C-c S-i**: Inspect a value
* **C-c C-m**: Macroexpand the call under the point
* **C-c C-d C-d**: Look up documentation for a var
* **C-c C-z**: Switch from a Clojure buffer to the repl buffer
* **C-c M-p**: Switch the repl namespace to match the current buffer
* **C-c C-w c**: List all callers of a given function

Pressing "v" on a stack trace a debug buffer will jump to the file and
line referenced by that frame if possible.

Note that SLIME was designed to work with Common Lisp, which has a
distinction between interpreted code and compiled code. Clojure has no
such distinction, so the load-file functionality is overloaded to add
<code>:reload-all</code> behaviour.

## Alternate Usage

There are other ways to use Swank for different specific
circumstances.  For each of these methods you will have to install the
slime and slime-repl Emacs Lisp libraries manually as outlined in
"Connecting with SLIME" below.

### Standalone Server

If you just want a standalone swank server with no third-party
libraries, you can use the shell wrapper that Leiningen installs for
you:

    $ lein plugin install swank-clojure 1.4.3
    $ ~/.lein/bin/swank-clojure

    M-x slime-connect

If you put `~/.lein/bin` on your <tt>$PATH</tt> it's even more
convenient.

### Manual Swank in Project

You can also start a swank server by hand from inside your project.
You'll need to have installed using `lein plugin
install`, then launch the server from the shell:

    $ lein swank # you can specify PORT and HOST optionally

If you're using Maven, add this to your pom.xml under the
\<dependencies\> section:

```xml
    <dependency>
      <groupId>swank-clojure</groupId>
      <artifactId>swank-clojure</artifactId>
      <version>1.4.3</version>
    </dependency>
```

Then you can launch a swank server like so:

    $ mvn clojure:swank

Note that due to a bug in clojure-maven-plugin, you currently cannot
include it as a test-scoped dependency; it must be compile-scoped. You
also cannot change the port from Maven; it's hard-coded to 4005.

### Embedding

You can embed Swank Clojure in your project, start the server from
within your own code, and connect via Emacs to that instance:

```clj
(ns my-app
  (:require [swank.swank]))
(swank.swank/start-server) ;; optionally takes :host/:port keyword args
```

To make this work in production, swank-clojure needs to be in
`:dependencies` in project.clj in addition to being installed
as a user-level plugin. If you do this, you can also start the server
directly from the `java` command-line launcher if you're using Clojure
1.3 or newer:

    $ java -cp my-project-standalone-1.0.0.jar clojure.main -m swank.swank

## Connecting with SLIME

If you're not using the `M-x clojure-jack-in` method mentioned
above, you'll have to install SLIME yourself. The easiest way is to
use package.el. If you are using Emacs 24 or the
[Emacs Starter Kit](http://github.com/technomancy/emacs-starter-kit),
then you have it already. If not, get it
[from Emacs's own repository](http://bit.ly/pkg-el23).

Then add Marmalade as an archive source in your Emacs config:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(package-initialize)
```

Evaluate that, then run <kbd>M-x package-refresh-contents</kbd> to
pull in the latest source lists. Then you can do <kbd>M-x
package-install</kbd> and choose <kbd>slime-repl</kbd>.

When you perform the installation, you will see warnings related to
the byte-compilation of the packages. This is **normal**; the packages
will work just fine even if there are problems byte-compiling it upon
installation.

Then you should be able to connect to the swank server you launched:

    M-x slime-connect

It will prompt you for your host (usually localhost) and port. It may
also warn you that your SLIME version doesn't match your Swank
version; this should be OK.

To get syntax highlighting in your repl buffer, use this elisp:

```lisp
(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))
```

To get colors in stack traces, load the elisp in
`src/swank/payload/slime-frame-colors.el` inside Emacs and use
`lein swank $PORT localhost :colors? true` to launch the swank server.

## Troubleshooting

Currently having multiple versions of swank-clojure on the classpath
can cause issues when running `lein swank` or `lein jack-in`. It's
recommended to not put swank-clojure in your `:dev-dependencies` but
run `lein plugin install` to have it installed globally for all
projects instead. This also means that people hacking on your project
won't have to pull it in if they are not Emacs users.

It's also possible for some packages to pull in old versions of
swank-clojure transitively, so check the `lib/` directory if
you are having issues. In particular, Incanter is known to exhibit
this problem. Judicious use of `:exclusions` make it work:

```clj
   :dependencies [[incanter "1.2.3" :exclusions [swank-clojure]]]
```

Since swank-clojure 1.3.4, having versions of clj-stacktrace older
than 0.2.1 in your project or user-level plugins will cause `Unable to
resolve symbol: pst-elem-str` errors. Keep in mind that user-level
plugins in `~/.lein/plugins` are uberjars in Leiningen 1.x, so it's
possible that one of your plugins (such as `lein-difftest` before
version 1.3.7) contains an old clj-stacktrace even if it doesn't have
its own file there. Specifying a newer version should be enough if
you're having trouble:

```clj
    :dependencies [[clj-stacktrace "0.2.4"]]
```

Having old versions of SLIME installed either manually or using a
system-wide package manager like apt-get may cause issues. Also the
official CVS version of SLIME is not supported; it often breaks
compatibility with Clojure. In addition, the `slime-clj` packages are
incompatible with swank-clojure.

It's possible to have Emacs configured for both Common Lisp and
Clojure if you defer loading of Slime until it's needed.
[This issue](https://github.com/technomancy/swank-clojure/issues/66)
has details on how that is done. It's not possible to have a single
instance of Emacs connect to both though.

Swank-clojure and SLIME are only tested with GNU Emacs; forks such as
Aquamacs and XEmacs may work but are not officially supported.

On Mac OS X, Emacs sessions launched from the GUI don't always respect
your configured $PATH. If Emacs can't find `lein`, you may need to
give it some help. The quickest way is probably to add this elisp to
your config:

```lisp
(setenv "PATH" (shell-command-to-string "echo $PATH"))
```

When using `clojure-jack-in`, standard out for the Leiningen process
appears in the `*swank*` buffer, but the `*out*` var gets rebound to a
writer that is able to redirect to the `*slime-repl*` buffer. So in
general most Clojure output will show up in your repl buffer just
fine, but for output coming from Java libraries you may need to check
the `*swank*` buffer.

## Cygwin

If you are running Emacs from Cygwin, you'll need to add the following to your 
.emacs.d/init.el file:

```lisp
(defun cyg-slime-to-lisp-translation (filename)
  (replace-regexp-in-string "\n" "" 
   (shell-command-to-string
     (format "cygpath.exe --windows %s" filename))))

(defun cyg-lisp-to-slime-translation (filename)
  (replace-regexp-in-string "\n" "" (shell-command-to-string
     (format "cygpath.exe --unix %s filename"))))

(setq slime-to-lisp-filename-function #'cyg-slime-to-lisp-translation)
(setq lisp-to-slime-filename-function #'cyg-lisp-to-slime-translation)
``` 

This is required because the jvm runs as a normal Windows exe and uses
Windows style paths rather than Cygwin unix style paths.

## How it Works

Swank Clojure is simply a server that communicates over the Slime
protocol with an Emacs process. As such it runs in a JVM process,
usually launched by Leiningen. Slime is a client that runs within
Emacs to communicate with Swank. You can start the two of them
separately as explained in "Connecting with SLIME" above, but `M-x
clojure-jack-in` will send the elisp code for Slime to the Emacs
process to ensure that it uses a version of Slime that is compatible
with that version of Swank. Once the Swank server is finished loading,
it sends a signal to Emacs to connect to it.

## Debugger

You can set repl-aware breakpoints using `swank.core/break`.
For now, see
[Hugo Duncan's blog](http://hugoduncan.org/post/2010/swank_clojure_gets_a_break_with_the_local_environment.xhtml)
for an explanation of this excellent feature.

[CDT](http://georgejahad.com/clojure/swank-cdt.html) (included in
Swank Clojure since 1.4.0) is a more comprehensive debugging tool
that includes support for stepping, seting breakpoints, catching
exceptions, and eval clojure expressions in the context of the current
lexical scope.

Note that the CDT does not work with `:eval-in-leiningen` without
extra manual configuration.

## TODO

* unmap-ns command
* show method argument names in slime inspector (theoretically possible?)
* show better metadata on functions in inspector
* offer restarts for class/var not found exceptions (slamhound integration?)
* add elisp payload for cdt commands
* suppress false "warning: unabled to add tools.jar to classpath" message

## Community

The [swank-clojure mailing list](http://groups.google.com/group/swank-clojure) 
and clojure channel on Freenode are the best places to bring up
questions/issues.

Contributions are preferred as either Github pull requests or using
"git format-patch". Please use standard indentation with no tabs,
trailing whitespace, or lines longer than 80 columns. See [this post
on submitting good patches](http://technomancy.us/135) for some
tips. If you've got some time on your hands, reading this [style
guide](http://mumble.net/~campbell/scheme/style.txt) wouldn't hurt
either.

## License

Copyright © 2008-2012 Jeffrey Chu, Phil Hagelberg, Hugo Duncan, and
contributors

Licensed under the EPL. (See the file COPYING.)
