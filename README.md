# RCF – turn your Rich Comment Forms into tests

RCF is a REPL-friendly Clojure/Script test macro and notation for describing what code does, or should do. We find it especially good for brainstorming. A well-formed idea, presented for consideration, comes in the form of an RCF.

# Usage

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

Tests are run when you send a file or form to your Clojure/Script REPL. In Cursive, that's cmd-shift-L to re-run the file.

```text
Loading src/example.cljc...
✅✅✅✅✅✅✅✅Loaded
```

# Configuration

`(tests)` blocks erase by default (macroexpanding to nothing). They will only run and assert under a flag:


```Clojure
; clj
(alter-var-root #'hyperfiddle.rcf/*enabled* (constantly true))

; cljs
(set! hyperfiddle.rcf/*enabled* true)
```

In ClojureScript, your build tool might reload namespaces, running tests when you save the file.
To prevent it:

```Clojure
(ns dev-entrypoint
  (:require [hyperfiddle.rcf :refer-macros [tests]]))

(defn ^:dev/before-load stop [] (set! hyperfiddle.rcf/*enabled* false))
(defn ^:dev/after-load start [] (set! hyperfiddle.rcf/*enabled* true))
```

Tests are always erased in cljs `:advanced` compilation mode.

The `:test` alias will generate clojure.test deftest vars for use in CI:

```bash
% clj -M:test -e "(require 'example)(clojure.test/run-tests 'example)"

Testing example
✅✅✅✅✅✅✅✅
Ran 1 tests containing 8 assertions.
0 failures, 0 errors.
{:test 1, :pass 8, :fail 0, :error 0, :type :summary}
```

# Fastest cljs example

```bash
% shadow-cljs watch :browser
# once ready, in another shell
% shadow-cljs browser-repl
# wait for the prompt
% cljs.user =>
```

```clojure
(require '[hyperfiddle.rcf :refer-macros [tests]])
(set! hyperfiddle.rcf/*enabled* true)
(tests 1 := 2)
```
Test results are also printed to the browser's javascript console.

# FAQ

*One of my tests threw an exception, but the stack trace is empty?* — you want `{:jvm-opts ["-XX:-OmitStackTraceInFastThrow"]}` [explanation](https://web.archive.org/web/20190416091616/http://yellerapp.com/posts/2015-05-11-clojure-no-stacktrace.html) (this may be JVM specific)

# Contributing

Sure – you can reach us on clojurians.net in #hyperfiddle or ping @dustingetz.

# Acknowledgments

Thank you to https://github.com/tristefigure for discovery, first implementations, and especially the work on the ClojureScript compiler monkey patches. RCF was not easy to write.

![Scroll Of Truth meme saying "you do not really understand something until you can explain it as a passing test".](./doc/meme.png)
