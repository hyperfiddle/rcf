(ns hyperfiddle.rcf
  #?(:cljs (:require-macros [hyperfiddle.rcf :refer [tests]]))
  (:require #?(:clj [hyperfiddle.rcf.impl :as impl])
            [clojure.test :as t]
            [hyperfiddle.rcf.reporters]
            [hyperfiddle.rcf.queue]
            [hyperfiddle.rcf.time]
            [hyperfiddle.rcf.unify]))

#?(:cljs (goog-define ^boolean ENABLED true))
#?(:cljs (goog-define ^boolean TIMEOUT 1000))
#?(:cljs (goog-define ^boolean GENERATE-TESTS false))

;; "Set to true if you want to generate clojure.test compatible tests. This
;; will define testing functions in your namespace using `deftest`. Defaults to
;; `false`.
#?(:clj  (def ^:dynamic *enabled* (= "true" (System/getProperty "hyperfiddle.rcf.enabled")))
   :cljs (def ^boolean ^:dynamic *enabled* ENABLED))

(defn enable! [& [v]]
  #?(:clj  (alter-var-root #'*enabled* (constantly (if (some? v) v true)))
     :cljs (set! *enabled* (if (some? v) v true))))

#?(:clj (def ^:dynamic *timeout* (or (System/getProperty "hyperfiddle.rcf.timeout") 1000))
   :cljs (def ^:dynamic *timeout* TIMEOUT))

(defn set-timeout! [ms]
  #?(:clj (alter-var-root #'*timeout* (constantly ms))
     :cljs (set! *timeout* ms)))

#?(:clj  (def ^:dynamic *generate-tests* (= "true" (System/getProperty "hyperfiddle.rcf.generate-tests")))
   :cljs (def ^boolean ^:dynamic *generate-tests* GENERATE-TESTS))

(def ^{:doc "
Function to push value to async queue, e.g. `(! 42)`. RCF redefines this var in tests context. For REPL
convenience, defaults to println outside of tests context."}
  ! println)

(def ^{:doc "Queue behaving as a value. Assert `% := _` to pop from it. Async, will time out after `:timeout` option, default to 1000 (ms)."}
  %)

(defmacro tests [& body]
  (if *generate-tests*
    `(deftest ~(gensym "rcf") ~@body)
    (when *enabled*
      (apply impl/tests &env body))))

(defmacro deftest
  "Like `clojure.test/deftest`, but:
   - will run when evaluated at the REPL,
   - var is tagged with the `:hyperfiddle/rcf` test selector.

  `clojure -M:test/cognitect --exclude :hyperfiddle/rcf`  will skip RCF tests"
  [name & body]
  (apply impl/deftest &env name body))

(defn- push-binding [q d] (let [[c b _a] q] [d c b]))

(defn push! [bindings val] (swap! bindings push-binding val))

#?(:clj (defmethod t/assert-expr := [msg form] (impl/assert-unify {:message msg} `= form)))
#?(:clj (defmethod t/assert-expr :<> [msg form] (impl/assert-unify {:message msg} `not= form)))
