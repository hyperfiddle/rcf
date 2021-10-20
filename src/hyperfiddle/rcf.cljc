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

(def ^{:doc "
Function to push value to async queue, e.g. `(! 42)`. RCF redefines this var in tests context. For REPL
convenience, defaults to println outside of tests context."}
  ! println)

(def ^{:doc "Queue behaving as a value. Assert `% := _` to pop from the it. Async, will time out after `:timeout` option, default to 1000 (ms)."} %)

(def ^{:doc "Queue backing `%`. Exposed to help you debug timing out tests."} q)

(s/def ::effect (s/and seq?
                       #(not (#{'let 'let* := :<> 'tests} (first %)))
                       (s/cat :body (s/+ ::expr))))

(s/def ::expr (s/or :tests  (s/cat :tests #{'tests}     :opts (s/? map?)  :body (s/* (s/or :effect ::effect, :expr ::expr)))
                    :assert (s/cat :eq    #{:= :<>}     :actual any?      :expected any?)
                    :let    (s/cat :let   #{'let 'let*} :bindings vector? :body (s/* ::expr))
                    :string string?
                    :value  any?))

(defn cljs? [env] (some? (:js-globals env)))

(defn prefix-sym [env x]
  (symbol (if (cljs? env) "cljs.test" "clojure.test") (name x)))

(defn rewrite-stars [body]
  (walk/postwalk #(case %
                    *1 `(get @~'RCF__bindings 0)
                    *2 `(get @~'RCF__bindings 1)
                    *3 `(get @~'RCF__bindings 2)
                    %)
                 body))

(defn replace* [smap coll] (walk/prewalk #(get smap % %) coll))

(defn spec->rcf [x] (replace* {::s/invalid ::invalid} x))
(defn rcf->spec [x] (replace* {::invalid ::s/invalid} x))

(defmacro try-expr
  "Used by the 'is' macro to catch unexpected exceptions.
  You don't call this."
  [menv msg form]
  (let [{:keys [file line end-line column end-column]} menv
        [_ type left right]                            form
        left#                                          (gensym "left")]
    `(let [!done#      (volatile! false)
           ~'RCF__done (fn [] (when-not @!done# (vswap! !done# not) (~'RCF__done)))]
       (try
         (let [~left#  ~(rewrite-stars left)
               result# (unifies? ~type (spec->rcf ~left#) ~(rewrite-stars right))]
           (if result#
             (~(prefix-sym &env 'do-report)
              {:type     ::pass,   :message ~msg,
               :file     ~file    :line    ~line :end-line ~end-line :column ~column :end-column ~end-column
               :expected '~(rcf->spec right), :actual  ~(rcf->spec left#)})
             (~(prefix-sym &env 'do-report)
              {:type     ::fail,   :message ~(str/triml (str msg " in " (rcf->spec left))),
               :file     ~file    :line    ~line  :end-line    ~end-line :column ~column :end-column ~end-column
               :expected '~(rcf->spec right), :actual  (rcf->spec ~left#) :assert-type ~type}))
           ~left#)
         (catch ~(if (cljs? &env) :default 'Throwable) t#
           (~(prefix-sym &env 'do-report)
            {:type       ::error, :message ~(str/triml (str msg " in " (::form (meta left) (rcf->spec left)))),
             :file       ~file
             :line       ~line
             :end-line   ~end-line
             :column     ~column
             :end-column ~end-column
             :expected   '~form,
             :actual     t#}))
         (finally
           (~'RCF__done))))))

(defn quoted? [x] (and (sequential? x) (= 'quote (first x))))

(defn rewrite-lvars [form]
  (walk/postwalk
   (fn [x]
     (if (and (symbol? x) (or (= '_ x) (= \? (first (name x)))))
       (list 'quote x)
       x))
   form))

(defmacro unifies? [type x pattern]
  (let [pattern (if (quoted? pattern) pattern (rewrite-lvars pattern))]
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

(defn push-binding [q d] (let [[c b _a] q] [d c b]))

(def phantom-effect? #{'*1 '*2 '*3})

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
  (let [counter (let [x (volatile! 0)] #(vswap! x inc))]
    (walk/postwalk #(case % _ (symbol (str "?_" (counter))) %) body)))

(defn rewrite-assert [menv type actual expected]
  `(try-expr ~(select-keys menv [:file :line :end-line :column :end-column :cljs]) nil (unifies? ~type ~actual ~expected)))

(defn var-name [var]
  (when var
    (let [{:keys [ns name]} (meta var)]
      (symbol (str ns) (str name)))))

(defn resolve'
  ([x] #?(:clj (var-name (resolve x))))
  ([env x] (:name (ana-api/resolve env x))))

(defn persents [env form]
  (let [resolver (if (cljs? env) (partial resolve' env) resolve')
        form     (walk/prewalk (fn [x]
                             (if (and (sequential? x) (#{'fn 'fn*} (first x)))
                               nil
                               x))
                           form)]
    (->> (tree-seq coll? identity form)
         (sequence (comp
                    (filter symbol?)
                    (map resolver)
                    (filter #{`%})))
         (seq))))

(defn replace-var
  ([env var-sym-map form]
   (replace-var env -1 var-sym-map form))
  ([env stop var-sym-map form]
   (let [!n-found (volatile! 0)]
     (walk/postwalk (fn [x]
                      (if-not (= stop @!n-found)
                        (if-some [var' (and (symbol? x) (if (cljs? env)
                                                          (resolve' env x)
                                                          (resolve' x)))]
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
    `(do (cljs.test/update-current-env! [:testing-contexts] (constantly (list ~doc)))
         ~@body)
    `(binding [t/*testing-contexts* (list ~doc)]
       ~@body)))

(defn rewrite-body [menv q exprs]
  (when-some [[[type val] & exprs] (seq exprs)]
    (case type
      :tests           (if-let [opts (:opts val)]
                         `((let [~'RCF__timeout_prev ~'RCF__timeout
                                 ~'RCF__timeout      ~(:timeout opts 1000)]
                             ~@(rewrite-body menv q (cons [type (dissoc val :opts)] exprs))))
                         (rewrite-body menv q (concat (:body val) exprs)))
      :assert          (let [{:keys [eq actual expected]} val]
                         (if (or (persents menv actual) (persents menv expected))
                           `(~(poll-n menv (count (persents menv actual)) q (if (phantom-effect? actual)
                                                                              (cons (rewrite-assert menv eq actual expected)
                                                                                    (rewrite-body menv q exprs))
                                                                              (push-binding! (rewrite-assert menv eq actual expected)
                                                                                             (rewrite-body menv q exprs)))))
                           (if (phantom-effect? actual)
                             (cons (rewrite-assert menv eq actual expected)
                                   (rewrite-body menv q exprs))
                             (push-binding! (rewrite-assert menv eq actual expected)
                                            (rewrite-body menv q exprs)))))
      :effect          (let [step (fn step [[[type val] & xs]]
                                    (case type
                                      (:string :value) (if (seq xs)
                                                         (cons val (step xs))
                                                         (list val))
                                      (rewrite-body menv q (cons [type val] xs))))
                             expr (or (:form val) (step (:body val)))]
                         (if (persents menv expr)
                           (list (poll-n menv (count (persents menv expr)) q
                                         (push-binding! (rewrite-stars expr) (rewrite-body menv q exprs))))
                           (push-binding! (rewrite-stars expr) (rewrite-body menv q exprs))))
      :string          `((testing' ~val ~@(rewrite-body menv q exprs)))
      :let             (let [{:keys [bindings body]} val
                             step                    (fn step [[[k v] & bindings] body]
                                                       (if (persents menv v)
                                                         (poll-n menv (count (persents menv v)) q `((let [~k ~v] ~@(if (empty? bindings) body (list (step bindings body))))))
                                                         `(let [~k ~v] ~@(if (empty? bindings) body (list (step bindings body))))))]
                         (concat (list (step (partition 2 bindings) (rewrite-body menv q body)))
                                 (rewrite-body menv q exprs)))
      :value             (if (persents menv val)
                           (list (poll-n menv (count (persents menv val)) q
                                         (push-binding! (rewrite-stars val) (rewrite-body menv q exprs))))
                           (push-binding! (rewrite-stars val) (rewrite-body menv q exprs)))
      :expr              (rewrite-body menv q (cons val exprs)))))

(defn filename->name [file]
  (-> (str/split file #"\.")
      (first)
      (str/replace #"_" "-")
      (str/replace #"/" "_")))

(defmacro deftest [nom q assert-count & body]
  (let [cljs? (cljs? &env)
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
                                      #_(~(prefix-sym &env 'do-report) {:type :end-test-var, :var '~nom})
                                      ~(when cljs?
                                         `(cljs.test/update-current-env! [:testing-contexts] clojure.core/empty))
                                      (~'RCF__async__done)))
                     ~'RCF__bindings (binding-queue)]
                 (binding [~@(when-not cljs? [(prefix-sym &env '*testing-vars*) `(conj ~(prefix-sym &env '*testing-vars*) '~nom)])]
                   (~(prefix-sym &env 'do-report) {:type :begin-test-var, :var '~nom})
                   (try (do ~@(replace-var &env {`! !, `q `(q/get-queue ~q)} body))
                        (catch ~(if cljs? 'js/Error 'Throwable) e#
                          (~(prefix-sym &env 'do-report) {:type     ::error,
                                               :message  "Uncaught exception, not in assertion."
                                               :expected nil,
                                               :actual   e#})))))]
    (cond
      cljs?            `(do (~(prefix-sym &env 'deftest) ~nom (cljs.test/async ~'RCF__async__done ~body))
                            (when *enabled*
                              (~nom)))
      *generate-tests* `(do (~(prefix-sym &env 'deftest) ~nom
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

(defn asserts [[exprs]] (filter #{:assert} (tree-seq nav-body nav-body exprs)))

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
          menv          (-> (meta &form)
                            (assoc :cljs (cljs? &env))
                            (update :file #(or % (str *ns*)))
                            (merge &env))
          parsed        [(->> (cons 'tests body)
                              rewrite-infix
                              (spec->rcf)
                              (s/conform ::expr))]
          q             (gensym "q")]
      (if (= ::s/invalid (first parsed))
        (throw (ex-info "Invalid syntax" (s/explain-data ::expr body)))
        `(deftest ~test-name-sym ~q ~(count (asserts parsed))
           (rcf->spec (do ~@(rewrite-body menv q parsed))))))))
