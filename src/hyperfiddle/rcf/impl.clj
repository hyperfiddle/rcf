(ns hyperfiddle.rcf.impl
  (:require [clojure.tools.analyzer.ast :as ast]
            [hyperfiddle.rcf.analyzer :as ana]
            [cljs.analyzer :as a]
            [clojure.string :as str]
            [clojure.test :as t]
            [hyperfiddle.rcf.queue :as q]
            [hyperfiddle.rcf.time :as time]
            [hyperfiddle.rcf.unify :as u :refer [unifier*]]
            [meander.epsilon :as m]
            [meander.strategy.epsilon :as m*]
            [hyperfiddle.rcf.reporters]))

(defn has-meta? [o] (instance? clojure.lang.IMeta o))

(defn walk [inner outer form]
  (cond
    (list? form)                            (outer form (apply list (map inner form)))
    (instance? clojure.lang.IMapEntry form) (outer form (clojure.lang.MapEntry/create (inner (key form)) (inner (val form))))
    (seq? form)                             (outer form (doall (map inner form)))
    (instance? clojure.lang.IRecord form)   (outer form (reduce (fn [r x] (conj r (inner x))) form form))
    (coll? form)                            (outer form (into (empty form) (map inner form)))
    :else                                   (outer form form)))

(defn forward-metas [form form']
  (if (has-meta? form')
    (with-meta form' (merge (meta form) (meta form')))
    form'))

(defn prewalk [f form]
  (if (reduced? form)
    (unreduced form)
    (unreduced (walk (partial prewalk f) forward-metas (f form)))))

(defn postwalk [f form]
  (if (reduced? form)
    (unreduced form)
    (unreduced (walk (partial postwalk f) (fn [form form'] (forward-metas form (f (forward-metas form form'))))
                     form))))

;; Print an ast node. Filters noisy info before printing.
#_(defn print-node [ast] (clojure.pprint/pprint (prewalk (fn [x] (if (and (map? x) (:op x)) (dissoc x :env :info) x)) ast)))

(defn lvar? [form]
  (or (= '_ form)
      (and (simple-symbol? form)
           (= \? (first (name form))))))

(defn sigil? [form] (contains? (methods t/assert-expr) form))

(def do-op? #{'tests 'do `cljs.test/async})

(defn rewrite-infix [form]
  ;; NOTE (& !head ?x (m/pred sigil? ?sigil) ?y & !tail) (& !head (is (?sigil ?x ?y)) &  !tail)
  (prewalk (fn [form]
             (m/match form
               ((m/pred do-op? ?op) & ?tail) (cons ?op (let [tail ?tail]
                                                         (loop [r                []
                                                                [a b c :as tail] tail]
                                                           (if (empty? tail)
                                                             (seq r)
                                                             (if (sigil? b)
                                                               (recur (conj r `(is (~b ~a ~c))) (drop 3 tail))
                                                               (recur (conj r a) (rest tail)))))))
               _ form))
           form))

(def rewrite-doc
  (m*/bottom-up (m*/attempt (m*/rewrite
                             ((m/pred do-op? ?op) & ?head (m/pred string? ?doc) & ?tail) (?op & ?head (`testing ?doc & ?tail))))))

(defn star-number [ns name]
  (let [star-n (case name "*1" 1 "*2" 2 "*3" 3 nil)]
    (if (some? ns)
      (and (#{"cljs.core" "clojure.core"} ns)
           star-n)
      star-n)))

(defn rewrite-star [form]
  (m/match form
    (m/symbol ?ns ?name) (if-let [num (star-number ?ns ?name)]
                           (with-meta `(get @~'RCF__bindings ~(dec num))
                             {::form form})
                           form)
    _ form))

(def rewrite-stars (m*/top-down (m*/attempt rewrite-star)))

(defn replace-n [from to n form]
  (let [seen  (atom 0)
        done? (fn [x] (if (= n @seen)
                        true
                        (do (when (= x from)  (swap! seen inc))
                            false)))]
    ((m*/top-down-until done? (m*/attempt (m*/rewrite ~from ~to)))
     form)))

(declare rewrite-%)

(defn poll-n [body]
  (let [% (gensym "%")]
    `(q/poll! ~'RCF__q ~'RCF__time_start (deref ~'RCF__timeout) :hyperfiddle.rcf/timeout
              (fn [~%] ~(rewrite-% (replace-n `hyperfiddle.rcf/% % 1 `(do ~@body)))))))

(defn count-%? [form] (count (filter #{`hyperfiddle.rcf/%} (tree-seq coll? identity form))))

(defn rewrite-% [form]
  (if-not (seq form)
    form
    (if (pos? (count-%? (first form)))
      (list (poll-n form))
      (cons (first form) (rewrite-% (rest form))))))

(defmacro binding-queue [] `(atom [nil nil nil]))

(defn push-binding [q d] (let [[c b _a] q] [d c b]))

(defn push-binding! [expr exprs]
  (if (empty? exprs)
    (list expr)
    `((first (swap! ~'RCF__bindings push-binding ~expr))
      ~@exprs)))

(defn do-not-push? [form]
  (and (seq? form)
       (#{'testing 'recur 'throw 'catch 'finally} (first form))))

(defn rewrite-push [forms]
  (if (empty? forms)
    ()
    (if (do-not-push? (first forms))
      (concat (list (first forms)) (rewrite-push (rest forms)))
      (push-binding! (first forms) (rewrite-push (rest forms))))))

(defn rewrite-push-binding [form]
  (postwalk (fn [form]
              (m/match form
                       ('quote & ?tail)   (:hyperfiddle.rcf.analyzer/form (meta form) form)
                       (do)               (do)
                       (do & ?tail)       (cons 'do (rewrite-push ?tail))
                       (`testing & ?tail) (cons `testing (rewrite-push ?tail))
                       _ form))
            form))

(defn rewrite-do [form]
  (m/match form
           ((m/pred do-op?) & _) (->> form
                                      (rewrite-infix)
                                      (rewrite-%)
                                      (rewrite-doc)
                                      ;; (rewrite-push-binding)
                                      (rewrite-stars))
           _ form))

(defn do-form? [form] (and (seq? form) (do-op? (first form))))

(defn flatten-top-level [form]
  (seq (reduce (fn [r x]
                 (cond
                   (do-form? x) (into r (flatten-top-level (rest x)))
                   (do-op? x)   (conj r 'do)
                   :else        (conj r x)))
               [] form)))

(defn simplify-do [form]
  (with-meta
    (seq (reduce (fn [r x]
                   (cond
                     (and (do-form? x) (= 1 (count (rest x)))) (into r (simplify-do (rest x)))
                     (seq? x)                                  (conj r (simplify-do x))
                     :else                                     (conj r x)))
                 [] form))
    (meta form)))

(defn rewrite* [subst-map form]
  (let [matches? #(contains? subst-map %)]
    (prewalk (fn [form]
               (if (matches? form)
                 (get subst-map form)
                 form))
             form)))

(defn recover-form-meta [form-map form]
  (letfn [(recover-form [form]
            (if-let [form' (or (get form-map form) (::form (meta form)))]
              (recover-form form')
              form))]
    (prewalk (fn [form]
               (if (has-meta? form)
                 (if-let [form' (recover-form form)]
                   (if (not= form form')
                     (vary-meta form assoc ::form form')
                     form)
                   form)
                 form))
             form)))

(defn async-tests [env body]
  (let [done (gensym "async_done_")]
    (if (ana/cljs? env)
      (concat `(cljs.test/async ~done)
              (rest body) ;; drop (do …)
              `((~done)))
      body)))

(defn autoquote-lvars [env ast]
  (ast/prewalk ast (fn [ast]
                     (case (:op ast)
                       :maybe-class (if (lvar? (:form ast))
                                      (assoc ast :class (list 'quote (:form ast)))
                                      ast)
                       :var         (if (ana/cljs? env)
                                      (if (lvar? (:form ast))
                                        (try (a/resolve-var (:env ast) (:form ast)
                                                            (a/confirm-var-exists-throw))
                                             ast
                                             (catch clojure.lang.ExceptionInfo _t
                                               (assoc ast :op ::lvar)))
                                        ast)
                                      ast)
                       ast))))

(def ast-passes [autoquote-lvars])

(defn tests-clj [&env & body]
  (let [[forms macroexpanded] (ana/macroexpand-all ast-passes {:locals &env} (flatten-top-level (cons 'tests body)))
        !                     (gensym "RCF__!")
        set-timeout!          (gensym "RCF__set-timeout!")
        rewritten             (->> macroexpanded
                                   (async-tests &env)
                                   (rewrite* {`hyperfiddle.rcf/!            !
                                              `hyperfiddle.rcf/set-timeout! set-timeout!})
                                   (simplify-do)
                                   (rewrite-do)
                                   ;; TODO *1 *2 *3 recover missing
                                   (recover-form-meta forms))]
    `(binding [*ns* (the-ns '~(symbol (str *ns*)))]
       (let [~'RCF__q          (q/queue)
             ~!                (~'fn [x#] #_(prn "!=>" x#) (q/offer! ~'RCF__q x#)) #_ (partial q/offer! ~'RCF__q)
             ~'RCF__time_start (time/current-time)
             ~'RCF__timeout    (atom hyperfiddle.rcf/*timeout*)
             ~set-timeout!     (partial reset! ~'RCF__timeout)
             ~'RCF__bindings   (binding-queue)]
         ~rewritten))))

(defn tests-cljs [&env & body]
  (let [[forms macroexpanded] (ana/macroexpand-all ast-passes &env #_{:locals &env} (flatten-top-level (cons 'tests body)))
        !                     (gensym "RCF__!")
        set-timeout!          (gensym "RCF__set-timeout!")
        rewritten             (->> macroexpanded
                                   (async-tests &env)
                                   (rewrite* {`hyperfiddle.rcf/!            !
                                              `hyperfiddle.rcf/set-timeout! set-timeout!})
                                   (simplify-do)
                                   (rewrite-do)
                                   ;; TODO *1 *2 *3 recover missing
                                   (recover-form-meta forms))]
    ;; (clojure.pprint/pprint macroexpanded)
    `(let [~'RCF__q          (q/queue)
           ~!                (~'fn [x#] #_(prn "!=>" x#) (q/offer! ~'RCF__q x#)) #_ (partial q/offer! ~'RCF__q)
           ~'RCF__time_start (time/current-time)
           ~'RCF__timeout    (atom hyperfiddle.rcf/*timeout*)
           ~set-timeout!     (partial reset! ~'RCF__timeout)
           ~'RCF__bindings   (binding-queue)]
       ~rewritten)))


(defn- stacktrace-file-and-line
  [stacktrace]
  (if (seq stacktrace)
    (let [^StackTraceElement s (first stacktrace)]
      {:file (.getFileName s) :line (.getLineNumber s)})
    {:file nil :line nil}))

(defn do-report-clj [m]
  (t/report
   (case
       (:type m)
     :hyperfiddle.rcf/fail (merge (stacktrace-file-and-line (drop-while
                                                             #(let [cl-name (.getClassName ^StackTraceElement %)]
                                                                (or (str/starts-with? cl-name "java.lang.")
                                                                    (str/starts-with? cl-name "clojure.test$")
                                                                    (str/starts-with? cl-name "hyperfiddle.rcf$")
                                                                    (str/starts-with? cl-name "hyperfiddle.rcf$")
                                                                    (str/starts-with? cl-name "clojure.core$ex_info")))
                                                             (.getStackTrace (Thread/currentThread)))) m)
     :hyperfiddle.rcf/error (merge (stacktrace-file-and-line (.getStackTrace ^Throwable (:actual m))) m)
     m)))

(defmacro do-report [m]
  (if (ana/cljs? &env)
    `(cljs.test/do-report ~m)
    `(do-report-clj ~m)))

(defmacro deftest-cljs [name & body]
  (when a/*load-tests*
    `(do
       (def ~(vary-meta name assoc :test `(fn [] ~@body))
         (fn [] (cljs.test/test-var (.-cljs$lang$var ~name))))
       (set! (.-cljs$lang$var ~name) (var ~name))
       #_(when *enabled* (t/test-var (var ~name))))))

(defn test-var [v]
  (when-let [t (:test (meta v))]
    (binding [t/*testing-vars* (conj t/*testing-vars* v)]
      (let [result (try (t)
                        (catch Throwable e
                          (do-report {:type :hyperfiddle.rcf/error, :message "Uncaught exception, not in assertion."
                                      :expected nil, :actual e, :ns *ns*})))]
        (t/do-report {:type :end-test-var, :var v})
        result))))

(defmacro deftest-clj [name & body]
  (when t/*load-tests*
    `(do (def ~(vary-meta name assoc :hyperfiddle/rcf true, :test `(fn ~name [] ~@body))
             (fn [] (test-var (var ~name))))
         (when hyperfiddle.rcf/*enabled* (test-var (var ~name))))))

(defn deftest [&env name & body]
  (if (ana/cljs? &env)
    `(deftest-cljs ~name ~(apply tests-cljs &env body))
    `(deftest-clj  ~name ~(apply tests-clj &env body))))

(defn original-form [form]
  (or (::form (meta form)) form))

(defn recover [form] (prewalk original-form form))

(defn assert-unify [pred msg [_op lhs rhs :as form]]
  `(let [lhs#           ~lhs
         rhs#           ~rhs
         [result# env#] (unifier* lhs# rhs#)]
     (if (~pred true (u/failed? env#))
       (do (do-report {:type     :hyperfiddle.rcf/fail, :message ~msg,
                       :expected '~(recover form),      :actual  { ;;:lhs       '~(original-form lhs)
                                                                  :lhs-value lhs#
                                                                  ;;:rhs       '~(original-form rhs)
                                                                  :rhs-value rhs#
                                                                  :env       env#}
                       :ns *ns*})
           lhs#)
       (do (do-report {:type     :hyperfiddle.rcf/pass, :message ~msg,
                       :expected '~(recover form),      :actual  result#
                       :ns *ns*})
           (if (u/failed? env#)
             lhs#
             result#)))))

(defmacro assert-expr [& args]
  (if (ana/cljs? &env)
    `(cljs.test/assert-expr ~@args)
    `(clojure.test/assert-expr ~@args)))

(defmacro try-expr* [msg form]
  `(try ~(assert-expr msg form)
        (catch ~(if (ana/cljs? &env) :default 'Throwable) err#
          (do-report {:type     :hyperfiddle.rcf/error, :message ~msg,
                      :expected '~form, :actual err#}))))

(defmacro is
  ([form] `(is ~form nil))
  ([form msg] `(try-expr* ~msg ~form)))

(defmacro testing [doc & body]
  (if (ana/cljs? &env)
    `(cljs.test/testing ~doc ~@body)
    `(clojure.test/testing ~doc ~@body)))

