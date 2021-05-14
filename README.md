# RCF – turn Rich Comment Forms into tests

REPL-friendly Clojure/Script macro and notation for describing what code does, or should do. We find it especially good for brainstorming. A well-formed idea, presented for consideration, comes in the form of an RCF.

# Usage

`(tests)` blocks macroexpand to nothing by default. They will run and assert under the following conditions:
* Send form to REPL (the macro detects this)
* Send file/buffer to REPL (in Cursive – cmd-shift-L which calls clojure.core/load-string)
* When run under JVM flag `-Dhyperfiddle.rcf.enabled=true`
* If configured `(hyperfiddle.rcf/set-config! {:enabled true})`

`(clojure.test/run-tests 'namespace)` works if enabled under this flag:
* `-Dhyperfiddle.rcf.generate-tests=true`
* `(hyperfiddle.rcf/set-config! {:generate-tests true})`

```Clojure
{:aliases {:dev  {:jvm-opts ["-Dhyperfiddle.rcf.enabled=true"]}
           :test {:jvm-opts ["-Dhyperfiddle.rcf.generate-tests=true"]}}}
```
```clojure
(ns user
  (:require [hyperfiddle.rcf :refer [tests]]))

(tests
  "infix `:=` is equality assertion"
  1 := 1 ; pass
  1 := 2 ; fail

  (tests
    "unification is supported with `:?=`"
    {:a 1, :b 2} :?= '{:a ?a, :b ?b}
    (:?= 1 '?a) ; fyi infix works
    (hyperfiddle.rcf/unifies? {:a 1, :b 2} '{:a ?a, :b ?a}) := false)
  
  "wildcards"
  {:a 1, :b [2 3]} :?= {:a _, _ [2 _]}

  "the usual REPL bindings"
  :foo
  :bar
  :baz
  *3 := :foo
  *2 := :bar
  *1 := :baz

  "effects"
  (doto (inc 1) prn)
  *1 := 2)
```

Send file to REPL (you can also send to REPL a single `(tests)` form):

```text
Loading user.cljc... 
✅❌✅✅✅✅✅✅✅Loaded
```

# FAQ

*One of my test threw an exception, but the stack trace is empty?* — [answer](https://web.archive.org/web/20190416091616/http://yellerapp.com/posts/2015-05-11-clojure-no-stacktrace.html)

# Contributing

Sure – you can reach us on clojurians.net in #hyperfiddle or ping @dustingetz.

# Acknowledgments

Thank you to https://github.com/tristefigure for discovery and first implementations. This library was not easy to write and we explored many approaches.

![Scroll Of Truth meme saying "you do not really understand something until you can explain it as a passing test".](./doc/meme.png)
