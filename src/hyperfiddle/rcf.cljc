(ns hyperfiddle.rcf
  (:refer-clojure :exclude [=])
  #?(:cljs (:require-macros [hyperfiddle.rcf :refer [tests deftest async]]
                            [hyperfiddle.rcf.impl :refer [make-queue]]
                            [cljs.test :as t :refer [assert-expr]]))
  (:require #?(:clj [hyperfiddle.rcf.impl :as impl])
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t])
            #?(:clj [cljs.test :as cljs-test])
            #?(:clj [hyperfiddle.rcf.analyzer :as ana])
            #?(:clj [clojure.walk :as walk])
            [clojure.string :as str]
            [hyperfiddle.rcf.reporters]
            [hyperfiddle.rcf.queue]
            [hyperfiddle.rcf.time]
            [hyperfiddle.rcf.unify :as u]))

(def = clojure.core/=)

#?(:cljs (goog-define ^boolean ENABLED false))
#?(:cljs (goog-define ^boolean TIMEOUT 400))
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

#?(:clj  (def ^:dynamic *generate-tests*  (= "true" (System/getProperty "hyperfiddle.rcf.generate-tests")))
   :cljs (def ^boolean ^:dynamic *generate-tests* GENERATE-TESTS))

(def ^{:doc "
Function to push value to async queue, e.g. `(! 42)`. RCF redefines this var in tests context. For REPL
convenience, defaults to println outside of tests context."}
  ! println)

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
  (cond
    *generate-tests* `(deftest ~(gen-name &form) ~@body)
    *enabled*        (impl/tests* &env body)
    :else            nil))

(defmacro deftest
  "When *load-tests* is false, deftest is ignored."
  [name & body]
  (if (:js-globals &env)
    `(cljs.test/deftest ~name ~(impl/tests* &env body))
    (when t/*load-tests*
      `(do (def ~(vary-meta name assoc :test `(fn [] ~(impl/tests* &env body)))
             (fn [] (impl/test-var (var ~name))))
           (when *enabled* (~name))))))

(defn done [])

(defmacro async [done & body]
  (if (ana/cljs? &env)
    `(cljs-test/async ~done ~@body)
    `(let [~done (constantly nil)]
       ~@body)))

(defn async-notifier [n done]
  (let [!seen (atom 0)]
    (fn []
      (swap! !seen inc)
      (when (= @!seen n)
        (done)))))

(defmacro make-queue [& args] (apply impl/make-queue args))

(defmacro is
  ([form] `(is ~form nil))
  ([form msg] `(try-expr ~msg ~form)))

(defmacro try-expr
  [msg form]
  (let [cljs? (ana/cljs? &env)
        {:keys [file line end-line column end-column]} (meta form)]
    `(try ~(if cljs?
             (cljs-test/assert-expr &env msg form)
             (t/assert-expr msg form))
          (catch ~(if cljs? :default 'Throwable) t#
            (do-report {:type :error, :message ~msg,
                        :file ~file :line ~line :end-line ~end-line :column ~column :end-column ~end-column
                        :expected '~form, :actual t#}))
          (finally
            (~'RCF__done!)))))

;; Same as default `=` behavior, but returns the first argument instead of a boolean.

(defmacro do-report [m]
  (if (:js-globals &env)
    `(cljs.test/do-report ~m)
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
#?(:clj (defmethod cljs-test/assert-expr 'hyperfiddle.rcf/= [menv msg form] (assert-= menv msg form)))

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
                            :actual   env#})
                lhs#))))))

#?(:clj (defmethod t/assert-expr :hyperfiddle.rcf/= [msg form] (assert-unify nil msg form)))
#?(:clj (defmethod cljs-test/assert-expr :hyperfiddle.rcf/= [menv msg form] (assert-unify menv msg form)))
