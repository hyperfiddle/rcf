# RCF – REPL-first async test macro for Clojure/Script

RCF turns your Rich Comment Forms into tests (in the same file as your functions). Send form or file to REPL to run tests and it squirts dopamine ✅✅✅. It's good, try it!

![](https://i.imgur.com/nBOOZq7.png)

Features
* Clojure/Script
* async tests
* zero boilerplate
* natural REPL workflow
* one key-chord to run tests, no hotkey configuring
* same-file tests (examples are better than docstrings)
* no file watchers, no extra windows, no beeping, no latency
* notebook support – [example NextJournal notebook](https://nextjournal.com/dustingetz/missionary-relieve-backpressure)
* it's fun! ✅✅✅

RCF is specifically engineered to support [Electric Clojure](https://github.com/hyperfiddle/electric), which we test, document and teach with RCF.

Hype quotes:
* "RCF has changed my habits with regards to tests. It is so much easier than flipping back and forth between files, you get my preferred work habits - work in a comment block until something works. But before RCF I never took the time to turn comment blocks into an automated test"
* "I use RCF to do leetcode style questions as 'fun practice.' It certainly didn't feel fun before!"
* "I think people make the mistake of comparing this with other methods of inlining tests near their function definitions. The integration with the REPL, low syntax/interface, reduces friction and makes testing more attractive as a language of communication and verification."
* "I used RCF in a successful interview. RCF was a massive help in communication and a fast tool for thought whilst under the conditions of technical interview."

# Dependency

Project maturity: CLJ is stable, CLJS is experimental, bb is experimental.

```clojure 
{:deps {com.hyperfiddle/rcf {:mvn/version "20220926-202227"}}}
```

Changelog
* :throws
* babashka support (experimental)
* **breaking** don't return final result return nil like comment
* `20220926-202227` `!` is deprecated, use `tap` instead
* `20220827-151056` async test forms no longer guaranteed return final result
* `20220405` maven group-id renamed from `hyperfiddle` to `com.hyperfiddle` for security
* 2021 Dec 18: clojurescript dependency is now under the :cljs alias, see #25
* 2021 Oct 20: custom reporters now dispatch on qualified keywords, see #19

Current dev priority is improving complex async tests in ClojureScript.

[![JVM](https://github.com/hyperfiddle/rcf/actions/workflows/tests_clj.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/rcf/actions/workflows/tests_clj.yml)
[![NodeJS](https://github.com/hyperfiddle/rcf/actions/workflows/tests_node.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/rcf/actions/workflows/tests_node.yml)
[![Browser](https://github.com/hyperfiddle/rcf/actions/workflows/tests_browser.yml/badge.svg?branch=master)](https://github.com/hyperfiddle/rcf/actions/workflows/tests_browser.yml)

# Usage

`(tests)` blocks erase by default (macroexpanding to nothing), which avoids a startup time performance penalty as well as keeps tests out of prod.

It's an easy one-liner to turn on tests in your dev entrypoint:

```clojure
(ns user ; user ns is loaded by REPL startup
  (:require [hyperfiddle.rcf]))

(hyperfiddle.rcf/enable!)
```
Tests are run when you send a file or form to your Clojure/Script REPL.

```clojure
(ns example
  (:require [hyperfiddle.rcf :refer [tests tap %]]))

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
  
  "multiple tests on one value"
  (def xs [:a :b :c])
  (count xs) := 3
  (last xs) := :c
  (let [xs (map identity xs)]
    (last xs) := :c
    (let [] (last xs) := :c))

  "exceptions"
  (assert false "boom") :throws java.lang.AssertionError

  (tests
    "nested tests (is there a strong use case?)"
    1 := 1)

  (tests
    "REPL bindings work"
    (keyword "a") := :a
    (keyword "b") := :b
    (keyword "c") := :c
    *1 := :c
    *2 := :b
    *3 := :a
    *1 := :c                   ; inspecting history does not affect history

    (keyword "d") := :d
    *1 := :d
    *2 := :c
    *3 := :b
    (symbol *2) := 'c          ; this does affect history
    (symbol *2) := 'd))
```
```text
Loading src/example.cljc...
✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅Loaded
```

# Async tests

```Clojure
(ns example
  (:require [clojure.core.async :refer [chan >! go go-loop <! timeout close!]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(rcf/set-timeout! 100)

(tests
  "async tests"
  #?(:clj  (tests
             (future
               (tap 1) (Thread/sleep 10)        ; tap value to queue
               (tap 2) (Thread/sleep 200)
               (tap 3))
             % := 1                               ; pop queue
             % := 2
             % := ::rcf/timeout)
     :cljs (tests
             (defn setTimeout [f ms] (js/setTimeout ms f))
             (tap 1) (setTimeout 10 (fn []
             (tap 2) (setTimeout 200 (fn []
             (tap 3)))))
             % := 1
             % := 2
             % := ::rcf/timeout)))

(tests 
  "electric"
  (def !x (atom 0))
  (def dispose
    (e/run
      (let [x (e/watch !x)
            a (inc x)
            b (inc x)]
        (tap (+ a b)))))
  % := 2
  (swap! !x inc)
  % := 4
  (swap! !x inc)
  % := 6
  (dispose))

(tests
  "core.async"
  (def c (chan))
  (go-loop [x (<! c)]
    (when x
      (<! (timeout 10))
      (tap x)
      (recur (<! c))))
  (go (>! c :hello) (>! c :world))
  % := :hello
  % := :world
  (close! c))

(tests
  "missionary"
  (def !x (atom 0))
  (def dispose ((m/reactor (m/stream! (m/ap (! (inc (m/?< (m/watch !x)))))))
                (fn [_] #_(prn ::done)) #(prn ::crash %)))
  % := 1
  (swap! !x inc)
  (swap! !x inc)
  % := 2
  % := 3
  (dispose)))
```

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

For CLJS tests to run, `rcf/enable!` must be true **in both CLJ (shadow-cljs macroexpansion time) and CLJS (JS runtime)**. Reports may be printed to **browser console instead of the REPL**, because browser REPLs donn't intercept the async println.

```Clojure
(ns dev-entrypoint
  (:require [example] ; transitive inline tests will erase
            [hyperfiddle.rcf :refer [tests]]))

; wait to enable tests until after app namespaces are loaded
(hyperfiddle.rcf/enable!)

; subsequent REPL interactions will run tests

; prevent test execution during cljs hot code reload
#?(:cljs (defn ^:dev/before-load stop [] (hyperfiddle.rcf/enable! false)))
#?(:cljs (defn ^:dev/after-load start [] (hyperfiddle.rcf/enable!)))
```

# FAQ

*One of my tests threw an exception, but the stack trace is empty?* — you want `{:jvm-opts ["-XX:-OmitStackTraceInFastThrow"]}` [explanation](https://web.archive.org/web/20190416091616/http://yellerapp.com/posts/2015-05-11-clojure-no-stacktrace.html) (this may be JVM specific)

*I see no output* — RCF is off by default, run `(hyperfiddle.rcf/enable!)`

*Emacs has no output and tests are enabled* — check if your emacs supports emojis

*How do I customize what’s printed at the REPL?* — see [reporters.clj](https://github.com/hyperfiddle/rcf/blob/03c821c3875c3dfe647c945430ecdc5a7c8b594f/src/hyperfiddle/rcf/reporters.clj), [reporters.cljs](https://github.com/hyperfiddle/rcf/blob/03c821c3875c3dfe647c945430ecdc5a7c8b594f/src/hyperfiddle/rcf/reporters.cljs)

*How do I run RCF tests in a polylith app via `clojure -Srepo -M:poly test` ?* see [https://github.com/ieugen/poly-rcf](https://github.com/ieugen/poly-rcf) for an example repo.
Short story: Add `:jvm-opts ["-Dhyperfiddle.rcf.generate-tests=true"]` to `:poly` alias in `./deps.edn` and for tests under `src/` add "src" to `:test :extra-paths` for each brick with tests.

# Community

&#35;hyperfiddle @ clojurians.net

![Scroll Of Truth meme saying "you do not really understand something until you can explain it as a passing test".](./doc/meme.jpg)
