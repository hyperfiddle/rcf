# RCF – a REPL-first test macro for Clojure/Script (experimental)

RCF turns your Rich Comment Forms into tests (in the same file as your functions). Send form or file to REPL to run tests and it squirts dopamine ✅✅✅. It's good, try it!

![](https://i.imgur.com/nBOOZq7.png)

* Natural workflow that encourages use of the REPL. This Is The Way.
* Documentation of example usages next to source code of function (this is way better than docstrings). Reading dense Clojure code is actually really hard and RCF fixes that.
* No watchers, no extra windows, no beeping, no lag.
* Tool for communication. While pairing on Zoom, bang out some assertions quickly, and right in the file you're working on. Watch your communication bandwidth improve.

# Dependency

Project maturity: experimental, expect severe breaking changes.

```Clojure
{:deps {hyperfiddle/rcf {:git/url "https://github.com/hyperfiddle/rcf.git" :sha ...}}}
```

[![JVM](https://github.com/hyperfiddle/rcf/actions/workflows/tests_clj.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/rcf/actions/workflows/tests_clj.yml)
[![NodeJS](https://github.com/hyperfiddle/rcf/actions/workflows/tests_node.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/rcf/actions/workflows/tests_node.yml)
[![Browser](https://github.com/hyperfiddle/rcf/actions/workflows/tests_browser.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/rcf/actions/workflows/tests_browser.yml)

# Usage

`(tests)` blocks erase by default (macroexpanding to nothing), which avoids a startup time performance penalty as well as keeps them out of prod.

It's an easy one-liner to turn on tests in your entrypoint:

```clojure
(ns user ; user is the default namespace loaded by REPL startup
  (:require [hyperfiddle.rcf]))

(hyperfiddle.rcf/enable!)
```
Tests are run when you send a file or form to your Clojure/Script REPL. In Cursive, that's cmd-shift-L to re-run the file.

```clojure
(ns example
  (:require [hyperfiddle.rcf :refer [tests]]))

(tests
  "equality"
  (inc 1) := 2

  "wildcards"
  {:a :b, :b [2 :b]} := {:a _, _ [2 _]}

  "unification"
  {:a :b, :b [2 :b]} := {:a ?b, ?b [2 ?b]}

  "unification on reference types"
  (def x (atom nil))
  {:a x, :b x} := {:a ?x, :b ?x}

  "the usual REPL bindings"
  :foo
  :bar
  :baz
  *3 := :foo
  *2 := :bar
  *1 := :baz

  (tests
    "nested tests for convenience"
    1 := 1))
```
```text
Loading src/example.cljc...
✅✅✅✅✅✅✅✅Loaded
```

# Async tests (experimental)

Coming soon!

# CI

To run in CI, configure a JVM flag for RCF to generate clojure.test deftests, and then run them with clojure.test. [Github actions example](https://github.com/hyperfiddle/rcf/tree/master/.github/workflows).

```Clojure
; deps.edn
{:aliases {:test {:jvm-opts ["-Dhyperfiddle.rcf.generate-tests=true"]}}}
```
```bash
% clj -M:test -e "(require 'example)(clojure.test/run-tests 'example)"

Testing example
✅✅✅✅✅✅✅✅
Ran 1 tests containing 8 assertions.
0 failures, 0 errors.
{:test 1, :pass 8, :fail 0, :error 0, :type :summary}
```

# ClojureScript configuration

```Clojure
(ns dev-entrypoint
  (:require [example] ; transitive inline tests will erase
            [hyperfiddle.rcf :refer [tests]]))

; wait to enable tests until after app namespaces are loaded
(hyperfiddle.rcf/enable!)

; subsequent REPL interactions will run tests

; prevent test execution during cljs hot code reload
#?(:cljs (defn ^:dev/before-load stop [] (hyperfiddle.rcf/enable!)))
#?(:cljs (defn ^:dev/after-load start [] (hyperfiddle.rcf/enable! false)))
```

# FAQ

*One of my tests threw an exception, but the stack trace is empty?* — you want `{:jvm-opts ["-XX:-OmitStackTraceInFastThrow"]}` [explanation](https://web.archive.org/web/20190416091616/http://yellerapp.com/posts/2015-05-11-clojure-no-stacktrace.html) (this may be JVM specific)

*I see no output* — RCF is off by default, run `(hyperfiddle.rcf/enable!)` to turn them on

*Emacs has no output and tests are enabled* — check if your emacs supports emojis

# Community

&#35;hyperfiddle @ clojurians.net

# Acknowledgments

Thank you to https://github.com/tristefigure for discovery and exploration of various ClojureScript compiler monkey patches that we considered in order to avoid the flag. RCF was not easy to make.

![Scroll Of Truth meme saying "you do not really understand something until you can explain it as a passing test".](./doc/meme.png)
