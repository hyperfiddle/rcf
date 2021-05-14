# Rich Comment Forms

  > An example is worth a thousand words.

  Examples you can copy/paste and run at the REPL. Helping communicate about
  code.

  - Clojure:          `(:require        [hyperfiddle.rcf :refer [tests]])`
  - ClojureScript: `(:require-macros [hyperfiddle.rcf :refer [tests]])`

  ```clojure
  (ns your-ns
    #?(:clj  (:require        [hyperfiddle.rcf :refer [tests]])
       :cljs (:require-macros [hyperfiddle.rcf :refer [tests]])))

  (comment
    (print "`comment` blocks expands to `nil`")
    (print "Nothing is evaluated here"))

  (tests
   "Rich Comments will evaluate only if enabled. They are not by default."
   (print "I won't print until you enable me, I'm just a comment so far.")
  ```

## Running Rich Comments

   Add this JVM option: `-Dhyperfiddle.rcf.enabled=true`¹ or run
   `(hyperfiddle.rcf/set-config! {:enabled true})`, then try to evaluate this:

   ```clojure
   (tests (println "I run"))
   ```

## Why is it called `tests`?

   Because you can assert equality in the Rich Comment.

   ```clojure
   (require '[hyperfiddle.rcf :as rcf :refer [tests]])

   (tests "Usage:"
     (tests "infix `:=` is equality assertion"
       1 := 1
       "foo" := "foo"
       (inc 0) := (dec 2))
     (tests "a prefix expression works as in Clojure"
       (= 1 1)  ; useful to eval at the repl
       (:= 1 1)) ; works too

     (tests "unification is supported with `:?=`"
       1 :?= '?a                           ; passes {?a 1}
       {:a 1, :b 2} :?= '{:a ?a, :b ?b}    ; passes {?a 1, ?b 2}
       (tests "this fails because 1 != 2"
         (rcf/unifies? {:a 1, :b 2} '{:a ?a, :b ?a}) := false
         (tests "and works as infix too"
           (:?= 1 '?a)))
       (tests "wildcard is supported with `_` and always unifies."
         {:a 1, :b 2} :?= '{:a _, :b _}))

     (tests "*1, *2 and *3 are respectively bound to the last, penultimate and antepenultimate values."
       :foo
       :bar
       :baz
       *3 := :foo
       *2 := :bar
       *1 := :baz)

     (tests "Everything that is not an assertion is considered an effect"
       (doto (inc 1) prn)
       *1 := 2)

     (rcf/with-config {:dots true}
       (tests "Theses will just print a single char: ✅ if they succeed"
         (:= 1 1)
         (:= 2 2))))
   ```

## Running tests

   By default, `tests` behaves as `comment` and has to be enabled specifically.

   If you want to work at the REPL, just eval the form.

   If you want to run them as part of a test suite, you can set
   `-Dhyperfiddle.rcf.generate-tests=true`¹ or
   `(hyperfiddle.rcf/set-config! {:generate-tests true})`. It will generate
   `clojure.test` compatible tests using `deftest`. You can then run them as
   you would do with `clojure.test`:

   `(clojure.test/run-tests 'namespace)`


## FAQ
### Why would you put tests next to your source?

   It's not about QA more than it is about communication by example.
    Separating tests from source is a common practice. I don't have any opinion
    on that matter.

   The `tests` macro was named `tests` and not `example` because it's
    familiar. `(tests 1 := 1)` is explicit and self-contained. It does test
    things. It's up to you to decide where to use it, and if you want these
    `tests` to be executed in QA. We iterate by writing examples using `tests`
    and then write the code in the same namespace. It gives us a tight
    feedback loop and clutter-free design space. Your approach might be
    different.

### Why not use `clojure.test` directly?

   Conciseness and crisp communication. RCF was born after uncountable hours
    of pair programming where screen-sharing and well-thought sentences were
    just not enough. Thinking in terms of test cases helped us to communicate
    with facts and reproducible conclusions. When a problem does not fit your
    working memory, laying it on paper/keyboard is a must. You want to remove
    as much friction and noise as possible in the process:

   - You don't want to teach your peers the intricacies of a testing framework,
   - you want to copy/paste easily,
   - just show that it runs.

> Just show me a passing test. — Dustin Getz's mantra


### One of my test threw an exception, but the stack trace is empty.

   Have a look [here](http://yellerapp.com/posts/2015-05-11-clojure-no-stacktrace.html). The cached version is [here](http://webcache.googleusercontent.com/search?q=cache:4Rya0J2jOb4J:yellerapp.com/posts/2015-05-11-clojure-no-stacktrace.html) if needed.

## Acknowledgments

  - RCF is an original idea from [@dustingetz](https://github.com/dustingetz).
  - [@tristefigure](https://github.com/tristefigure) wrote the first working
    versions (called Minitest) and did a great and creative job. We learned the
    intricacies, dead-ends, and advantages of various approaches from Minitest
    before settling on the current implementation.
  - RCF use a cljs-compatible, slightly modified version of
    [core.unify](https://github.com/clojure/core.unify).

## Contributing

   It's still early, but feel free to open issues for bugs or suggestions.
   Constructive feedback is always welcome.

----

¹: In `deps.edn`:
   ```clojure
   {:aliases {:dev  {:jvm-opts ["-Dhyperfiddle.rcf.enabled=true"]}
              :test {:jvm-opts ["-Dhyperfiddle.rcf.generate-tests=true"]}}}
   ```
----

![Scroll Of Truth meme saying "you do not really understand something until you can explain it as a passing test".](./doc/meme.png)
