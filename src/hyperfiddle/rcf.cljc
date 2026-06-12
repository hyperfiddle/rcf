(ns hyperfiddle.rcf
  (:refer-clojure :exclude [=])
  #?(:cljs (:require-macros [hyperfiddle.rcf :refer [tests deftest async]]
                            [hyperfiddle.rcf.impl :refer [make-queue]]))
  (:require #?(:clj [hyperfiddle.rcf.impl :as impl])
            #?(:clj [cloroutine.core])  ; rcf#56: load the `cr` macro into the compiler so emitted (cloroutine.core/cr …) resolves in user nses
            [cloroutine.impl]           ; rcf#56: state-machine trampoline (cljs runtime) + safe/hint (compile)
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t])
            #?(:clj [hyperfiddle.rcf.analyzer :as ana])
            #?(:clj [clojure.walk :as walk])
            [clojure.string :as str]
            [hyperfiddle.rcf.reporters :as reporters]
            [hyperfiddle.rcf.queue]
            [hyperfiddle.rcf.time]
            [hyperfiddle.rcf.unify :as u]))

(def = clojure.core/=)

#?(:cljs (goog-define ^boolean ENABLED false))
#?(:cljs (goog-define ^boolean TIMEOUT 400))

;; "Set to true if you want to generate clojure.test compatible tests. This
;; will define testing functions in your namespace using `deftest`. Defaults to
;; `false`.
#?(:clj  (defonce ^:dynamic *enabled* (= "true" (System/getProperty "hyperfiddle.rcf.enabled")))
   :cljs (def ^boolean ^:dynamic *enabled* ENABLED))

(defn enable! [& [v]]
  #?(:clj  (alter-var-root #'*enabled* (constantly (if (some? v) v true)))
     :cljs (set! *enabled* (if (some? v) v true))))

(defn enabled?
  "True when RCF currently runs `tests` blocks (reads the dynamic `*enabled*`)."
  []
  *enabled*)

#?(:clj
   (defn run-tests!
     "Run `ns-sym`'s rcf tests on the JVM; returns
      `{:ns ns-sym :pass N :fail [entry …]}` where entry =
      `{:file :line :column :doc :test-var :expected :actual}`.

      Reloads the ns under a binding-scoped enable — the `*enabled*` root is never
      touched, so nothing leaks into other sessions or hot reloads. The reporter
      active at call time still prints as usual; the returned map is additive.
      A `tests` block that binds its own reporter (`with-reporter`) bypasses the
      capture and is not counted. Side effect: `def`s inside `tests` blocks
      remount into the ns and persist.

      JVM-only: an nREPL eval compiles the `:clj` arms of a .cljc — a namespace's
      cljs tests are compiled and run by the cljs build (browser / karma), not here."
     [ns-sym]
     (let [!acc   (atom {:pass 0 :fails []})
           active (get @reporters/reporter-registry reporters/*reporter-key*)
           ;; count via compact's block-state accumulator, print via the active
           ;; reporter — without running compact's fns twice when it IS the active.
           count-and-print
           (fn [count-f print-f]
             (if (identical? count-f print-f)
               count-f
               (fn [m] (count-f m) (print-f m))))
           capture
           {:pass    (count-and-print (:pass reporters/compact-reporter) (:pass active))
            :fail    (count-and-print (:fail reporters/compact-reporter) (:fail active))
            :summary (fn [m]            ; per-block summary: accumulate, then delegate
                       (swap! !acc #(-> %
                                        (update :pass + (:pass m 0))
                                        (update :fails into (:fails m))))
                       ((:summary active) m))}]
       (reporters/register-reporter! :hyperfiddle.rcf/run-capture capture)
       (reporters/with-reporter :hyperfiddle.rcf/run-capture
         (binding [*enabled* true]
           (require ns-sym :reload)))
       (let [{:keys [pass fails]} @!acc]
         {:ns ns-sym :pass pass :fail (vec fails)}))))

#?(:clj (def ^:dynamic *timeout* (or (System/getProperty "hyperfiddle.rcf.timeout") 1000))
   :cljs (def ^:dynamic *timeout* TIMEOUT))

(defn set-timeout! [ms]
  #?(:clj (alter-var-root #'*timeout* (constantly ms))
     :cljs (set! *timeout* ms)))

#?(:clj  (def ^:dynamic *generate-tests*  (= "true" (System/getProperty "hyperfiddle.rcf.generate-tests"))))

(def ^{:doc "
Function to push value to async queue, e.g. `(tap 42)`. RCF redefines this var in tests context. For REPL
convenience, defaults to println outside of tests context."}
  tap (fn [x] (doto x println)))

(def ^{:doc "Deprecated alias for `tap`." :deprecated true} ! tap)

(comment
  "tap outside of tests macro"
  (is (= (with-out-str (tap 1)) "1\n")))

(def ^{:doc "Queue behaving as a value. Assert `% := _` to pop from it. Async, will time out after `:timeout` option, default to 1000 (ms)."}
  %)

(defn- push-binding [q d] (let [[c b _a] q] [d c b]))

(defn binding-queue []
  (let [!q    (atom [nil nil nil])
        push! (partial swap! !q push-binding)
        peek! #(get (deref !q) %)]
    [push! peek!]))

(defn gen-name [form]
  (let [{:keys [line _column]} (meta form)
        file (str/replace (name (ns-name *ns*)) #"[-\.]" "_")]
    (symbol (str "generated__" file "_" line))))

(defmacro tests [& body]
  (let [body `(~@body nil) ; return nil like comment, unlike do
        name (gen-name &form)]
    (cond
      *generate-tests*  `(deftest ~name ~@body)
      *enabled*         (if (:js-globals &env)
                          `(do (defn ~name []
                                 (cljs.test/update-current-env!
                                   [:hyperfiddle.rcf.reporters/block-state]
                                   (fn [_#] (atom {:pass 0 :fail 0 :fails []})))
                                 ~(impl/tests* &env body))
                               (when *enabled* (cljs.test/run-block (cljs.test/test-var-block* (var ~name) ~name))))
                          `(binding [hyperfiddle.rcf.reporters/*block-state* (atom {:pass 0 :fail 0 :fails []})]
                             (try ~(impl/tests* &env body)
                                  (finally (t/report (assoc @hyperfiddle.rcf.reporters/*block-state*
                                                            :type :hyperfiddle.rcf/summary))))))
      :else             nil)))

(defmacro deftest
  "When *load-tests* is false, deftest is ignored."
  [name & body]
  (if (:js-globals &env)
    `(do (cljs.test/deftest ~name ~(impl/tests* &env body))
         (when *enabled* (~name)))
    (when t/*load-tests*
      `(do (def ~(vary-meta name assoc :test `(fn [] ~(impl/tests* &env body)))
             (fn [] (impl/test-var (var ~name))))
           (when *enabled* (~name))))))

(defn done [])

(defmacro async [done & body]
  (if (ana/cljs? &env)
    `(cljs.test/async ~done ~@body)
    `(let [~done (constantly nil)]
       ~@body)))

(defn once
  "Wrap `f` so it runs at most once. Fires cljs.test `done` exactly once at the end of
   the coroutine (rcf#56), guarding against a double-fire from convergent branches."
  [f]
  (let [!fired (atom false)]
    (fn [] (when (compare-and-set! !fired false true) (f)))))

;; rcf#56 — async-% driver over cloroutine (cljs only).
;; A test body's `%` compiles to a `(park-tap)` suspension; `cloroutine.core/cr`
;; turns the body into a state machine that sequences control flow across it
;; (if/cond/loop/recur/try incl. catch); this driver feeds the async queue's
;; values back into the machine. Mirrors missionary's park/unpark over `cr`.
;; Single-threaded: a synchronously-available value re-enters the coroutine inline
;; (its next-block state is already set), so no scheduling is needed.
#?(:cljs
   (do
     (def ^:dynamic *poll-1*
       "Bound by `drive-tap` to the test's queue poll fn `(fn [n cb])`; `park-tap` calls it with n=1." nil)
     (def ^:dynamic *resume*
       "Bound by `drive-tap` to a 1-arg fn re-entering the coroutine with a delivered value." nil)
     (def ^:dynamic *tapped*
       "Holds the value the queue delivered for the in-flight `park-tap`; read by `unpark-tap`." nil)

     (defn park-tap
       "Suspend var (rcf#56): register a one-shot poll of the async queue, then suspend.
        The poll's callback re-enters the coroutine via `*resume*`."
       [] (*poll-1* 1 *resume*) nil)

     (defn unpark-tap
       "Resume var (rcf#56): the value the queue delivered for the matching `park-tap`."
       [] *tapped*)

     (defn drive-tap
       "Run an async-% test `coroutine`: start it, and each time the queue yields, re-enter
        with that value bound, until the body completes (its tail calls `RCF__done!`).
        `poll-1` is the test's `(fn [n cb])` queue poll."
       [poll-1 coroutine]
       (let [resume (fn resume [v] (binding [*poll-1* poll-1, *resume* resume, *tapped* v] (coroutine)))]
         (binding [*poll-1* poll-1, *resume* resume, *tapped* nil] (coroutine))))))

(defmacro make-queue [& args] (apply impl/make-queue args))

(defmacro is
  ([form] `(is ~form nil))
  ([form msg] `(try-expr ~msg ~form)))

(defmacro try-expr
  [msg form]
  (let [cljs? (ana/cljs? &env)
        {:keys [file line end-line column end-column]} (meta form)]
    `(try ~(t/assert-expr msg form)
          (catch ~(if cljs? :default 'Throwable) t#
            (do-report {:type :error, :message ~msg,
                        :file ~file :line ~line :end-line ~end-line :column ~column :end-column ~end-column
                        :expected '~form, :actual t#})))))

;; Same as default `=` behavior, but returns the first argument instead of a boolean.

(defmacro do-report [m]
  (if (:js-globals &env)
    ;; rcf-typed fails bypass cljs.test's file/line stitch (it matches :fail/:error
    ;; only) — stitch from the JS stack at the assertion site, like impl/do-report*
    ;; does from the JVM stack on clj. Explicit keys in m win (merge is right-biased).
    `(let [m# ~m]
       (cljs.test/do-report
         (if (= :hyperfiddle.rcf/fail (:type m#))
           (merge (cljs.test/file-and-line (js/Error.) 1) m#)
           m#)))
    `(impl/do-report ~m)))

#?(:clj (defn- assert-= [menv msg form]
          (let [[_= & args] form
                form        (cons '= (map impl/original-form args))]
            `(let [values# (list ~@args)
                   result# (apply = values#)]
               (if result#
                 (do-report {:type     :hyperfiddle.rcf/pass
                             :message  ~msg,
                             :expected '~form
                             :actual   (cons '= values#)})
                 (do-report {:type     :hyperfiddle.rcf/fail
                             :message  ~msg,
                             :expected '~form
                             :actual   (list '~'not (cons '~'= values#))}))
               (first values#)))))

#?(:clj (defmethod t/assert-expr 'hyperfiddle.rcf/= [msg form] (assert-= nil msg form)))

#?(:clj
   (defn- assert-unify [menv msg form]
     (let [[_= & args] form
           form        (cons := (map impl/original-form args))]
       `(let [lhs#           (identity ~(first args))
              rhs#           (identity ~(second args))
              [result# env#] (u/unifier* lhs# rhs#)]
          (if-not (u/failed? env#)
            (do (do-report {:type     :hyperfiddle.rcf/pass
                            :message  ~msg,
                            :expected '~form
                            :actual   result#})
                result#)
            (do (do-report {:type     :hyperfiddle.rcf/fail
                            :message  ~msg,
                            :expected '~form
                            :actual   (u/explain env#)})
                lhs#))))))

#?(:clj (defmethod t/assert-expr :hyperfiddle.rcf/= [msg form] (assert-unify nil msg form)))

(defmacro with
  "Resource cleanup helper, based on missionary's dependency-free Task protocol, see https://github.com/leonoel/task"
  [dispose-fn & body]
  `(let [dispose# ~dispose-fn]
     (try (do ~@body) (finally (dispose#)))))
