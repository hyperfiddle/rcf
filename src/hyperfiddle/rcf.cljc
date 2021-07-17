(ns hyperfiddle.rcf
  (:refer-clojure :exclude [*1 *2 *3])
  #?(:cljs (:require-macros [hyperfiddle.rcf :refer [tests]]))
  (:require #?(:clj [clojure.test :as t :refer [testing]]
               :cljs [cljs.test :as t :refer-macros [testing]])
            #?(:cljs [cljs.analyzer.api :as ana-api])
            [cljs.analyzer :as ana]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hyperfiddle.rcf.reporters]
            [hyperfiddle.rcf.unify :refer [unifier]]))

;; "Set this to true if you want to generate clojure.test compatible tests. This
;; will define testing functions in your namespace using `deftest`. Defaults to
;; `false`.

#?(:clj  (def ^:dynamic *enabled* (= "true" (System/getProperty "hyperfiddle.rcf.enabled")))
   :cljs (goog-define ^boolean ^:dynamic *enabled* false))


#?(:clj  (def ^:dynamic *generate-tests* (= "true" (System/getProperty "hyperfiddle.rcf.generate-tests")))
   :cljs (goog-define ^boolean ^:dynamic *generate-tests* false))


(s/def ::expr (s/or :assert (s/cat :eq #{:= :<>} :actual any? :expected any?)
                    :tests (s/cat :tests #{'tests} :doc (s/? string?) :body (s/* ::expr))
                    :effect (s/cat :do #{'do} :body (s/* any?))
                    :doc    string?))

(defn prefix-sym [cljs? x]
  (symbol (if cljs? "cljs.test" "clojure.test")
          (name x)))

(defmulti assert-expr
  (fn [menv msg form]
    (cond
      (nil? form) :always-fail
      (seq? form) (first form)
      :else       :default)))

#?(:clj (defn get-possibly-unbound-var
          "Like var-get but returns nil if the var is unbound."
          {:added "1.1"}
          [v]
          (try (var-get v)
               (catch IllegalStateException e
                 nil))))

#?(:clj (defn function?
          "Returns true if argument is a function or a symbol that resolves to
  a function (not a macro)."
          {:added "1.1"}
          [x]
          (if (symbol? x)
            (when-let [v (resolve x)]
              (when-let [value (get-possibly-unbound-var v)]
                (and (fn? value)
                     (not (:macro (meta v))))))
            (fn? x)))
   :cljs (defn function?
           "Returns true if argument is a function or a symbol that resolves to
  a function (not a macro)."
           [menv x]
           (and (symbol? x) (:fn-var (ana-api/resolve menv x)))))

(defn assert-predicate
  "Returns generic assertion code for any functional predicate.  The
  'expected' argument to 'report' will contains the original form, the
  'actual' argument will contain the form with all its sub-forms
  evaluated.  If the predicate returns false, the 'actual' form will
  be wrapped in (not...)."
  [menv msg form]
  (let [args (rest form)
        pred (first form)
        {:keys [file line end-line column end-column cljs]} menv]
    `(let [values# (list ~@args)
           result# (apply ~pred values#)]
       (if result#
         (~(prefix-sym cljs 'do-report)
          {:type :pass, :message ~msg,
           :file ~file :line ~line :end-line ~end-line :column ~column :end-column ~end-column
           :expected '~form, :actual (cons '~pred values#)})
         (~(prefix-sym cljs 'do-report)
          {:type :fail, :message ~msg,
           :file ~file :line ~line :end-line ~end-line :column ~column :end-column ~end-column
           :expected '~form, :actual (list '~'not (cons '~pred values#))}))
       result#)))

(defn assert-any
  "Returns generic assertion code for any test, including macros, Java
  method calls, or isolated symbols."
  [menv msg form]
  (let [{:keys [file line end-line column end-column]} menv]
    `(let [value# ~form]
       (if value#
         (~(prefix-sym (:cljs menv) 'do-report)
          {:type :pass, :message ~msg,
           :file ~file :line ~line :end-line ~end-line :column ~column :end-column ~end-column
           :expected '~form, :actual value#})
         (~(prefix-sym (:cljs menv) 'do-report)
          {:type :fail, :message ~msg,
           :file ~file :line ~line :end-line ~end-line :column ~column :end-column ~end-column
           :expected '~form, :actual value#}))
       value#)))

(defmethod assert-expr :default [menv msg form]
  (if (and (sequential? form)
           #?(:clj  (function? (first form))
              :cljs (function? menv (first form))))
    (assert-predicate menv msg form)
    (assert-any menv msg form)))

(defmethod assert-expr `unifies? [menv msg form]
  (let [{:keys [file line end-line column end-column]} menv
        type                                           (nth form 1)
        left                                           (nth form 2)
        original-left                                  (::form (meta left) left)
        right                                          (nth form 3)]
    `(let [left#   ~left
           result# (unifies? ~type left# ~right)]
       (if result#
         (~(prefix-sym (:cljs menv) 'do-report)
          {:type     :pass,  :message ~msg,
           :file     ~file   :line    ~line :end-line ~end-line :column ~column :end-column ~end-column
           :expected '~right, :actual  left#})
         (~(prefix-sym (:cljs menv) 'do-report)
          {:type     :fail,           :message ~(str msg " in " original-left),
           :file     ~file            :line    ~line :end-line ~end-line :column ~column :end-column ~end-column
           :expected '~right, :actual  left#, :assert-type ~type}))
       result#)))

(defn cljs? [env] (some? (:ns env)))

(defmacro try-expr
  "Used by the 'is' macro to catch unexpected exceptions.
  You don't call this."
  [menv msg form]
  (let [cljs? (cljs? &env)
        err   (if cljs? :default 'Throwable)
        {:keys [file line end-line column end-column]} menv]
    `(try
       ~(assert-expr menv msg form)
       (catch ~err t#
         (~(prefix-sym cljs? 'do-report)
          {:type       :error, :message ~msg,
           :file       ~file
           :line       ~line
           :end-line   ~end-line
           :column     ~column
           :end-column ~end-column
           :expected   '~form,
           :actual     t#})))))

(defmacro is
  ([menv form]     `(is ~menv ~form nil))
  ([menv form msg] `(try-expr ~menv ~msg ~form)))

(declare rewrite-body*)

(defmacro unifies? [type x pattern]
  `(let [x# ~x]
     (~(case type
         :=  `=
         :<> `not=)
      x# (unifier x# ~(clojure.walk/postwalk
                         (fn [x]
                           (if (and (symbol? x) (or (= '_ x) (= \? (first (name x)))))
                             (list 'quote x)
                             x))
                         pattern)))))

(defn rewrite-infix [body]
  (seq (loop [[x & xs :as body] body
              acc               []]
         (cond
           (empty? body)              acc
           (= 'tests x)               (recur xs (conj acc x))
           (and (sequential? x)
                (= 'tests (first x))) (recur xs (conj acc (rewrite-infix x)))
           (and (sequential? x)
                (= := (first x)))     (recur xs (conj acc x))
           (#{:= :<>} (first xs))     (recur (rest (rest xs)) (conj acc (list (first xs) x (first (rest xs)))))
           (string? x)                (recur xs (conj acc x))
           :else                      (recur xs (conj acc (list 'do x)))
           ))))

(def ^:dynamic *1)
(def ^:dynamic *2)
(def ^:dynamic *3)

(defn push-value! [expr exprs]
  `((binding [*3 *2,
              *2 *1,
              *1 (do ~expr)]
      ~@exprs)))

(defn rewrite-stars [body]
  (walk/postwalk #(case % *1 `*1, *2 `*2, *3 `*3, %) body))

(defn rewrite-wildcards [body]
  (let [counter (let [x (volatile! 0)] (fn [] (vswap! x inc)))]
    (walk/postwalk #(case % _ (symbol (str "?_" (counter))) %) body)))

(def not-doc? (complement #(= :doc (first %))))

(defn rewrite-body* [menv symf body]
  (seq
   (loop [[[type val :as expr] & body] body
          acc                          []]
     (if (nil? expr)
       acc
       (case type
         :assert (let [{:keys [eq actual expected]} val
                       actual                       (rewrite-stars actual)
                       expected                     (rewrite-stars expected)]
                   (recur body (conj acc `(is ~menv ~(with-meta `(unifies? ~eq ~actual ~(rewrite-wildcards expected)) {::form `~actual})))))
         :tests  (if-let [doc (:doc val)]
                   (recur body (conj acc `(~(symf 'testing) ~doc ~@(rewrite-body* menv symf (:body val)))))
                   (recur body (apply conj acc (rewrite-body* menv symf (:body val)))))
         :effect (recur nil (apply conj acc (push-value! (first (:body val)) (rewrite-body* menv symf (rewrite-stars body)))))
         :doc    (recur (drop-while not-doc? body)
                        (conj acc `(~(symf 'testing) ~val ~@(rewrite-body* menv symf (rewrite-stars (take-while not-doc? body)))))))))))

(defn rewrite-body [menv symf body]
  (let [parsed (s/conform ::expr (rewrite-infix (cons 'tests body)))]
    (if (= ::s/invalid parsed)
      (throw (ex-info "Invalid syntax" (s/explain-data ::expr body)))
      (rewrite-body* menv symf (list parsed)))))

(defn filename->name [file]
  (-> (str/split file #"\.")
      (first)
      (str/replace #"_" "-")
      (str/replace #"/" "_")))

(defmacro deftest [nom & body]
  (let [cljs? (cljs? &env)
        symf  (partial prefix-sym cljs?)]
    (if (or *generate-tests* cljs?)
      `(do (~(symf 'deftest) ~nom ~@body)
           (when *enabled*
             (~nom)))
       `((fn [] (binding [t/*testing-vars* (conj clojure.test/*testing-vars* '~nom)]
                 (clojure.test/do-report {:type :begin-test-var, :var '~nom})
                (clojure.test/inc-report-counter :test)
                (try (do ~@body)
                     (catch ~(if cljs? 'js/Error 'Throwable) e#
                       (~(symf 'do-report) {:type     :error,
                                            :message  "Uncaught exception, not in assertion."
                                            :expected nil,
                                            :actual   e#}))))
           (t/do-report {:type :end-test-var, :var '~nom}) )))))

(defn should-run-tests? [menv]
  (if (cljs? menv)
    ana/*load-tests*
    #?(:clj (and t/*load-tests* ; standard clojure.test way of skipping tests
                 (not *compile-files*) ; no tests in AOT compiled code
                 (or *enabled* *generate-tests*)))))

(defmacro tests
  {:style/indent [:defn]}
  [& body]
  (when (should-run-tests? &env)
    (let [prefix        "RCF"
          file          (:file (meta &form))
          line          (:line (meta &form))
          test-name-sym (-> (if (some? line)
                              (cond-> prefix
                                (some? file) (str "__" (filename->name file))
                                true         (str "__" line))
                              (gensym prefix))
                            (symbol)
                            (with-meta (assoc (meta &form)
                                              :ns (str *ns*))))
          cljs?         (some? (:ns &env))
          symf          (partial prefix-sym cljs?)
          menv          (-> (meta &form)
                            (assoc :cljs cljs?)
                            (update :file #(or % (str *ns*))))]
      `(deftest ~test-name-sym
         ~@(rewrite-body menv symf body)))))
