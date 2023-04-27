(ns hyperfiddle.rcf
  (:refer-clojure :exclude [=])
  #?(:cljs (:require-macros [hyperfiddle.rcf :refer [tests deftest async]]
                            [hyperfiddle.rcf.impl :refer [make-queue]]))
  (:require #?(:clj [hyperfiddle.rcf.impl :as impl])
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t])
            #?(:clj [hyperfiddle.rcf.analyzer :as ana])
            #?(:clj [clojure.walk :as walk])
            #?(:clj [clojure.java.io :as io])
            [clojure.string :as str]
            [hyperfiddle.rcf.reporters]
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

(defn ns-filename "Given a symbol identifying a namespace, return the corresponding file path"
  [sym]
  (-> (name sym)
    (str/replace #"\." "/")
    (str/replace #"-" "_")))

#?(:clj (defn find-file [relative-path]
          (when-let [res (io/resource relative-path)]
            (try (io/file res)
                 (catch IllegalArgumentException _
                   ;; resource is not a file on the classpath. E.g. jar:// sources are
                   ;; not files. We also don’t want to reload them.
                   nil)))))
#?(:clj
   (defn resolve-file
     "Resolve a source file from namespace symbol.
      Precedence:
      - cljc,
      - cljs if we are compiling clojurescript,
      - clj otherwise."
     [env ns-sym]
     (let [file-name (ns-filename ns-sym)
           cljc      (find-file (str file-name ".cljc"))
           clj       (find-file (str file-name ".clj"))
           cljs      (find-file (str file-name ".cljs"))]
       (if (and cljc (.exists cljc))
         cljc
         (if (:js-globals env)
           (when (and cljs (.exists cljs))
             cljs)
           (when (and clj (.exists clj))
             clj))))))
#?(:clj
   (defn is-ns-in-current-project? [env ns-sym]
     (let [current-dir (System/getProperty "user.dir")]
       (when-some [file (resolve-file env ns-sym)]
         (str/starts-with? (.getPath file) current-dir)))))

(defmacro tests [& body]
  (let [name (gen-name &form)
        ns (if (:js-globals &env)
             (:name (:ns &env))
             (:ns &env (.getName *ns*)))]
    (cond
      (and *generate-tests* (is-ns-in-current-project? &env ns)) `(deftest ~name ~@body)
      *enabled*         (if (:js-globals &env)
                          `(do (defn ~name [] ~(impl/tests* &env body))
                               (when *enabled* (cljs.test/run-block (cljs.test/test-var-block* (var ~name) ~name))))
                          (impl/tests* &env body))
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
    `(try ~(t/assert-expr msg form)
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
