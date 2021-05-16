Project status, 2021 May: closed beta, public release eta 2021

# RCF – turn your Rich Comment Forms into tests

RCF is a REPL-friendly Clojure/Script test macro and notation for describing what code does, or should do. We find it especially good for brainstorming. A well-formed idea, presented for consideration, comes in the form of an RCF.

# Usage

```clojure
(ns example
  (:require [hyperfiddle.rcf :refer [tests]]))

(tests
  "equality"
  1 := 1 ; pass
  1 := 2 ; fail
  
  "wildcards"
  {:a 1, :b [2 3]} := {:a _, _ [2 _]}

  "unification"
  {:a 1, :b 1} := {:a ?a, :b ?b} ; fail
  
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
✅✅✅✅
❌ example:10 
Usage: infix `:=` is equality assertion

(hyperfiddle.rcf/unifies? 1 2)
 := 
false

✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅Loaded
```

# Configuration

`(tests)` blocks erase by default (macroexpanding to nothing). They will only run and assert under a flag:

```Clojure
; deps.edn
{:aliases {:dev  {:jvm-opts ["-Dhyperfiddle.rcf.enabled=true"]}
           :test {:jvm-opts ["-Dhyperfiddle.rcf.generate-tests=true"]}}}
```

Unfortunately, this will run all your tests at startup when the namespaces load, which is too slow. To prevent this, wrap your dev entrypoint like below. Subsequent REPL interactions will still run tests.

```Clojure
; dev entrypoint
(ns dev (:require hyperfiddle.rcf))
(hyperfiddle.rcf/with-config {:enabled false} ; skip tests on app startup
  (require 'example))
```

We explored fixing the startup problem with monkeypatches to clojure.core/load and the ClojureScript module loader. We determined the monkeypatch approach to be workable, but not worth deploying yet as the flag is good enough for now.

The :test alias will generate clojure.test deftest vars for use in CI:

```bash
% clj -M:test -e "(require 'example)(clojure.test/run-tests 'example)"

Testing example
✅
❌ example:10
Usage: infix `:=` is equality assertion

(hyperfiddle.rcf/unifies? 1 2)
 :=
false

✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅✅
Ran 4 tests containing 20 assertions.
1 failures, 0 errors.
{:test 4, :pass 19, :fail 1, :error 0, :type :summary}
```

# FAQ

*One of my tests threw an exception, but the stack trace is empty?* — you want `{:jvm-opts ["-XX:-OmitStackTraceInFastThrow"]}` [explanation](https://web.archive.org/web/20190416091616/http://yellerapp.com/posts/2015-05-11-clojure-no-stacktrace.html) (this may be JVM specific)

Fixtures?

# Contributing

Sure – you can reach us on clojurians.net in #hyperfiddle or ping @dustingetz.

# Acknowledgments

Thank you to https://github.com/tristefigure for discovery, first implementations, and especially the work on the ClojureScript compiler monkey patches. RCF was not easy to write.

![Scroll Of Truth meme saying "you do not really understand something until you can explain it as a passing test".](./doc/meme.png)
