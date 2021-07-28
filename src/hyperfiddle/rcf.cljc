(ns hyperfiddle.rcf
  (:refer-clojure :exclude [*1 *2 *3])
  #?(:cljs (:require-macros [hyperfiddle.rcf :refer [tests]]))
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t])
            [cljs.analyzer.api :as ana-api]
            [cljs.analyzer :as ana]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hyperfiddle.rcf.reporters]
            [hyperfiddle.rcf.unify :refer [unifier]]
            [hyperfiddle.rcf.queue :as q]
            [hyperfiddle.rcf.time :as time]))

#?(:cljs (goog-define ^boolean ENABLED false))

;; "Set this to true if you want to generate clojure.test compatible tests. This
;; will define testing functions in your namespace using `deftest`. Defaults to
;; `false`.
#?(:clj  (def ^:dynamic *enabled* (= "true" (System/getProperty "hyperfiddle.rcf.enabled")))
   :cljs (def ^boolean ^:dynamic *enabled* ENABLED))

(defn enable! [& [v]]
  #?(:clj  (alter-var-root #'hyperfiddle.rcf/*enabled* (constantly (if (some? v) v true)))
     :cljs (set! *enabled* (if (some? v) v true))))

#?(:clj  (def ^:dynamic *generate-tests* (= "true" (System/getProperty "hyperfiddle.rcf.generate-tests")))
   :cljs (goog-define ^boolean ^:dynamic *generate-tests* false))

#?(:clj (def ^:dynamic *timeout* (or (System/getProperty "hyperfiddle.rcf.timeout") 1000))
   :cljs (goog-define ^:dynamic *timeout* 1000))

(defn set-timeout! [ms]
  #?(:clj (alter-var-root #'*timeout* (constantly ms))
     :cljs (set! *timeout* ms)))

(def ^{:doc "Function. Use it to push a value onto the RCF queue `(! 1)`"}
  !)

(def ^{:doc "Queue behaving as a value. Assert `% := _` to pop from the it. Async, will time out after `:timeout` option, default to 1000 (ms)."}
  %)

(def ^{:doc "Queue backing `%`. Exposed to help you debug timing out tests."}
  q)

(def ^:dynamic *doc* nil)

(s/def ::effect (s/and seq?
                       #(not (#{'let 'let* := :<> 'tests} (first %)))
                       (s/cat :body (s/+ ::expr))))

(s/def ::expr (s/or :tests (s/cat :tests #{'tests} :opts (s/? map?) :body (s/* (s/or :effect ::effect, :expr ::expr)))
                    :assert (s/cat :eq #{:= :<>} :actual any? :expected any?)
                    :let (s/cat :let #{'let 'let*} :bindings vector? :body (s/* ::expr))
                    :string string?
                    :value any?))

(defn prefix-sym [cljs? x]
  (symbol (if cljs? "cljs.test" "clojure.test")
          (name x)))

(defn rewrite-stars [body]
  (walk/postwalk #(case %
                    *1 `(get-binding! ~'RCF__bindings 0)
                    *2 `(get-binding! ~'RCF__bindings 1)
                    *3 `(get-binding! ~'RCF__bindings 2)
                    %)
                 body))

(defn assert-expr [menv msg form]
  (let [{:keys [file line end-line column end-column]} menv
        [_ type left right]                            form
        left (::form (meta left) left)
        left#                                          (gensym "left")]
    `(let [~left#  ~(rewrite-stars left)
           result# (unifies? ~type ~left# ~(rewrite-stars right))]
       (if result#
         (~(prefix-sym (:cljs menv) 'do-report)
          {:type     :pass,   :message ~msg,
           :file     ~file    :line    ~line :end-line ~end-line :column ~column :end-column ~end-column
           :expected '~right, :actual  ~left#
           :doc      *doc*})
         (~(prefix-sym (:cljs menv) 'do-report)
          {:type     :fail,   :message ~(str msg " in " left),
           :file     ~file    :line    ~line  :end-line    ~end-line :column ~column :end-column ~end-column
           :expected '~right, :actual  ~left# :assert-type ~type
           :doc      *doc*}))
       ~left#)))

(defn cljs? [env] (some? (:js-globals env)))

(defmacro try-expr
  "Used by the 'is' macro to catch unexpected exceptions.
  You don't call this."
  [menv msg form]
  (let [cljs?                                          (cljs? &env)
        err                                            (if cljs? :default 'Throwable)
        {:keys [file line end-line column end-column]} menv
        left                                           (nth form 1)]
    `(let [!done#      (volatile! false)
           ~'RCF__done (fn [] (when-not @!done# (vswap! !done# not) (~'RCF__done)))]
       (try
         ~(assert-expr menv msg form)
         (catch ~err t#
           (~(prefix-sym cljs? 'do-report)
            {:type       :error, :message ~(str msg " in " (::form (meta left) left)),
             :file       ~file
             :line       ~line
             :end-line   ~end-line
             :column     ~column
             :end-column ~end-column
             :expected   '~form,
             :actual     t#
             :doc        *doc*}))
         (finally
           (~'RCF__done))))))

(defmacro is
  ([menv form]     `(is ~menv ~form nil))
  ([menv form msg] `(try-expr ~menv ~msg ~form)))

(defn quoted? [x] (and (sequential? x) (= 'quote (first x))))

(defmacro unifies? [type x pattern]
  (let [pattern (if (quoted? pattern)
                  pattern
                  (clojure.walk/postwalk
                   (fn [x]
                     (if (and (symbol? x) (or (= '_ x) (= \? (first (name x)))))
                       (list 'quote x)
                       x))
                   pattern))]
    `(let [x# ~x]
       (~(case type
           :=  `=
           :<> `not=)
        x# (unifier x# ~pattern)))))

(def assert-form? #{:= :<>})

(defn rewrite-infix [form]
  (loop [acc                   []
         [a b c & xs :as form] form]
    (if (empty? form)
      (seq acc)
      (cond
        (= 'fn* a)    (let [body (rewrite-infix c)]
                        (if (and (seq? body)
                                 (seq? (first body))
                                 (assert-form? (ffirst body)))
                          (apply list a b body)
                          (list a b body)))
        (assert-form? b) (recur (conj acc (list b a c)) xs)
        :else
        (if (seq? a)
          (recur (conj acc (rewrite-infix a)) (rest form))
          (recur (conj acc a) (rest form)))))))

(defn binding-queue [] (atom [nil nil nil]))

(defn push-binding [q v] (let [[a b _c] q] [v a b]))

(defn get-binding! [q idx] (get @q idx))

(defn phantom-effect? [expr]
  (#{'*1 '*2 '*3} expr))

(defn push-binding! [expr exprs]
  (if (empty? exprs)
    `(~expr)
    (if (phantom-effect? expr)
      `((do ~expr
            ~@exprs))
      `((let [x# ~expr]
          (swap! ~'RCF__bindings #(push-binding % x#))
          ~@exprs)))))

(defn rewrite-wildcards [body]
  (let [counter (let [x (volatile! 0)] (fn [] (vswap! x inc)))]
    (walk/postwalk #(case % _ (symbol (str "?_" (counter))) %) body)))

(defn rewrite-assert [menv type actual expected]
  `(is ~(select-keys menv [:file :line :end-line :column :end-column :cljs]) (unifies? ~type ~actual ~(rewrite-wildcards expected))))

(defn var-name [var]
  (when var
    (let [{:keys [ns name]} (meta var)]
      (symbol (str ns) (str name)))))

(defn resolve'
  ([x] #?(:clj (var-name (resolve x))))
  ([env x] (:name (ana-api/resolve env x))))

(defn persents [env form]
  (let [resolver (if (cljs? env) (partial resolve' env) resolve')
        form (walk/prewalk (fn [x]
                             (if (and (sequential? x) (#{'fn 'fn*} (first x)))
                               nil
                               x))
                           form)]
    (->> (tree-seq coll? identity form)
         (filter symbol?)
         (map resolver)
         (filter #{`%}))))

(def has-%? (comp seq persents))

(defn replace-var
  ([env var-sym-map form]
   (replace-var env -1 var-sym-map form))
  ([env stop var-sym-map form]
   (let [!n-found (volatile! 0)]
     (walk/postwalk (fn [x]
                      (if-not (= stop @!n-found)
                        (if-some [var' (and (symbol? x) (or (#{'def} x)
                                                            (if (cljs? env)
                                                              (resolve' env x)
                                                              (resolve' x))))]
                          (if-let [sym (get var-sym-map var')]
                            (do (vswap! !n-found inc) sym)
                            x)
                          x)
                        x))
                    form))))

(defn poll-n
  ([env n q form]
   (poll-n env n q (gensym "%") form))
  ([env n q sym form]
   (if (zero? n)
     `(do ~@form)
     `(q/poll! ~q ~'RCF__time_start ~'RCF__timeout ::timeout
               (fn [~sym]
                 ~(poll-n env (dec n) q (replace-var env 1 {`% sym} form)))))))

(defmacro testing' [doc & body]
  (if (cljs? &env)
    `(do (cljs.test/update-current-env! [:doc] (constantly ~doc))
         ~@body)
    `(binding [*doc* ~doc]
       ~@body)))

(defn rewrite-body [menv symf q exprs]
  (when-some [[[type val] & exprs] (seq exprs)]
    (case type
      :tests           (if-let [opts (:opts val)]
                         `((let [~'RCF__timeout_prev ~'RCF__timeout
                                 ~'RCF__timeout      ~(:timeout opts 1000)]
                             ~@(rewrite-body menv symf q (cons [type (dissoc val :opts)] exprs))))
                         (rewrite-body menv symf q (concat (:body val) exprs)))
      :assert          (let [{:keys [eq actual expected]} val]
                         (if (or (has-%? menv actual) (has-%? menv expected))
                           `(~(poll-n menv (count (persents menv actual)) q (if (phantom-effect? actual)
                                                                              (cons (rewrite-assert menv eq actual expected)
                                                                                    (rewrite-body menv symf q exprs))
                                                                              (push-binding! (rewrite-assert menv eq actual expected)
                                                                                             (rewrite-body menv symf q exprs)))))
                           (if (phantom-effect? actual)
                             (cons (rewrite-assert menv eq actual expected)
                                   (rewrite-body menv symf q exprs))
                             (push-binding! (rewrite-assert menv eq actual expected)
                                            (rewrite-body menv symf q exprs)))))
      :effect          (let [step (fn step [[[type val] & xs]]
                                    (case type
                                      (:string :value) (if (seq xs)
                                                         (cons val (step xs))
                                                         (list val))
                                      (rewrite-body menv symf q (cons [type val] xs))))
                             expr (or (:form val) (step (:body val)))]
                         (if (has-%? menv expr)
                           (list (poll-n menv (count (persents menv expr)) q
                                         (push-binding! (rewrite-stars expr) (rewrite-body menv symf q exprs))))
                           (push-binding! (rewrite-stars expr) (rewrite-body menv symf q exprs))))
      :string          `((testing' ~val ~@(rewrite-body menv symf q exprs)))
      :let             (let [{:keys [bindings body]} val
                             step                    (fn step [[[k v] & bindings] body]
                                                       (if (has-%? menv v)
                                                         (poll-n menv (count (persents menv v)) q `((let [~k ~v] ~@(if (empty? bindings) body (list (step bindings body))))))
                                                         `(let [~k ~v] ~@(if (empty? bindings) body (list (step bindings body))))))]
                         (concat (list (step (partition 2 bindings) (rewrite-body menv symf q body)))
                                 (rewrite-body menv symf q exprs)))
      :value             (if (has-%? menv val)
                           (list (poll-n menv (count (persents menv val)) q
                                         (push-binding! (rewrite-stars val) (rewrite-body menv symf q exprs))))
                           (push-binding! (rewrite-stars val) (rewrite-body menv symf q exprs)))
      :expr              (rewrite-body menv symf q (cons val exprs)))))

(defn filename->name [file]
  (-> (str/split file #"\.")
      (first)
      (str/replace #"_" "-")
      (str/replace #"/" "_")))

(defmacro deftest [nom q assert-count & body]
  (let [cljs? (cljs? &env)
        symf  (partial prefix-sym cljs?)
        !     (gensym "!")
        body  `(let [!done-count# (atom 0)
                     ~q           (q/queue)
                     ~!           #(q/offer! ~q %)
                     ~'RCF__time_start (time/current-time)
                     ~'RCF__timeout ~*timeout*
                     ~'RCF__timeout_prev ~'RCF__timeout
                     ~'RCF__done  (fn []
                                    (swap! !done-count# inc)
                                    (when (= ~assert-count @!done-count#)
                                      #_(~(symf 'do-report) {:type :end-test-var, :var '~nom})
                                      ~(when cljs?
                                         `(cljs.test/update-current-env! [:testing-contexts] clojure.core/empty))
                                      (~'RCF__async__done)))
                     ~'RCF__bindings (binding-queue)]
                 (binding [~@(when-not cljs? [(symf '*testing-vars*) `(conj ~(symf '*testing-vars*) '~nom)])]
                   (~(symf 'do-report) {:type :begin-test-var, :var '~nom})
                   (try (do ~@(replace-var &env {`! !, `q `(q/get-queue ~q)} body))
                        (catch ~(if cljs? 'js/Error 'Throwable) e#
                          (~(symf 'do-report) {:type     :error,
                                               :message  "Uncaught exception, not in assertion."
                                               :expected nil,
                                               :actual   e#})))))]
    (cond
      cljs?            `(do (~(symf 'deftest) ~nom (cljs.test/async ~'RCF__async__done ~body))
                            (when *enabled*
                              (~nom)))
      *generate-tests* `(do (~(symf 'deftest) ~nom
                                              (let [~'RCF__async__done #()]
                                                ~body))
                            (when *enabled*
                              (~nom)))
      :else            `((fn [] (let [~'RCF__async__done #()]
                                  ~body))))))

(defn should-run-tests? [menv]
  (if (cljs? menv)
    ana/*load-tests*
    #?(:clj (and t/*load-tests* ; standard clojure.test way of skipping tests
                 (not *compile-files*) ; no tests in AOT compiled code
                 (or *enabled* *generate-tests*)))))

(defn- nav-body [x]
  (if (vector? x)
    (let [[type val] x]
      (case type
        (:expr :effect) val
        (:body val)))
    (:body x)))

(defn asserts [[exprs]]
  (->> (tree-seq nav-body nav-body exprs)
       (filter #{:assert})))

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
          cljs?         (cljs? &env)
          symf          (partial prefix-sym cljs?)
          menv          (-> (meta &form)
                            (assoc :cljs cljs?)
                            (update :file #(or % (str *ns*)))
                            (merge &env))
          parsed        [(->> (cons 'tests body)
                              rewrite-infix
                              (s/conform ::expr))]
          q             (gensym "q")]
      (if (= ::s/invalid (first parsed))
        (throw (ex-info "Invalid syntax" (s/explain-data ::expr body)))
        `(deftest ~test-name-sym ~q ~(count (asserts parsed))
           ~@(rewrite-body menv symf q parsed))))))
