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

(def do-op? #{'tests 'do 'do* `cljs.test/async `testing})

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

(defn rewrite-doc* [form]
  (cond (empty? form)          form
        (string? (first form)) `((testing ~(str/join " " (take-while string? form))
                                   ~@(take-while string? form)
                                   ~@(rewrite-doc* (->> form (drop-while string?) (take-while (complement string?)))))
                                 ~@(rewrite-doc* (->> form (drop-while string?) (drop-while (complement string?)))))
        :else                  (cons (first form) (rewrite-doc* (rest form)))))

(defn rewrite-doc [form]
  (postwalk (fn [form]
              (if (seq? form)
                (let [[op & exprs] form]
                  (if (#{'do 'do*} op)
                    (cons op (rewrite-doc* (rest form)))
                    form))
                form))
            form))

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
  (letfn [(step [exprs]
            (cond (empty? exprs)                  exprs
                  (pos? (count-%? (first exprs))) (list (poll-n exprs))
                  :else                           (cons (first exprs) (step (rest exprs)))))]
    (postwalk (fn [form]
                (if (seq? form)
                  (let [[op & exprs] form]
                    (if (do-op? op)
                      (cons op (step exprs))
                      form))
                  form))
              form)))

(defmacro binding-queue [] `(atom [nil nil nil]))

(defn push-binding! [expr exprs]
  (if (empty? exprs)
    (list expr)
    `((~'RCF__push! ~expr)
      ~@exprs)))

(defn do-not-push? [form]
  (and (seq? form)
       (#{'testing 'recur 'throw 'catch 'finally} (first form))))

(defn rewrite-push [forms]
  (if (empty? forms)
    ()
    (cond (do-not-push? (first forms)) (concat (list (first forms)) (rewrite-push (rest forms)))
          :else                        (push-binding! (first forms) (rewrite-push (rest forms))))))

;; TODO rework
(defn rewrite-push-binding [form]
  (postwalk (fn [form]
              (if (seq? form)
                (cond
                  (do-op? (first form))
                  ,, (m/match form
                       (hyperfiddle.rcf.impl/testing (m/pred string? ?doc) & ?tail)
                       ,, (cons `testing (cons ?doc (rewrite-push ?tail)))

                       (cljs.test/async ?done & ?tail)
                       ,, (cons `cljs.test/async (cons ?done (rewrite-push ?tail)))

                       ((m/pred do-op? ?op) & ?tail) (cons ?op (rewrite-push ?tail))
                       _ form)
                  (= 'quote (first form)) (:hyperfiddle.rcf.analyzer/form (meta form) form)
                  :else                   form)
                form))
            form))

(defn rewrite-do [form]
  (m/match form
           ((m/pred do-op?) & _) (->> form
                                      (rewrite-infix)
                                      (rewrite-%)
                                      (rewrite-doc)
                                      (rewrite-push-binding)
                                      (rewrite-stars))
           _ form))

(defn do-form? [form] (and (seq? form) (do-op? (first form))))

(defn flatten-top-level [form]
  (seq (reduce (fn [r x]
                 (cond
                   (do-form? x) (into r (flatten-top-level (rest x)))
                   (= 'tests x) (conj r 'do*)
                   :else        (conj r x)))
               [] form)))

(defn do*->do [form] (postwalk (fn [form] (if (= 'do* form) 'do form)) form))

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

(defn autoquote-lvars [env ast]
  (ast/postwalk ast (fn [ast]
                      ;; (prn (:op ast))
                      (case (:op ast)
                        :quote      (if (lvar? (:form (:expr ast)))
                                      (update ast :expr update :form #(list 'quote %))
                                      ast)
                        :maybe-class (if (lvar? (:form ast))
                                       (assoc ast :class (list 'quote (:form ast)))
                                       ast)
                        :var         (if (ana/cljs? env)
                                       (if (lvar? (:form ast))
                                         (try (a/resolve-var (:env ast) (:form ast)
                                                             (a/confirm-var-exists-throw))
                                              ast
                                              (catch clojure.lang.ExceptionInfo _t
                                                (assoc ast :op ::ana/lvar)))
                                         ast)
                                       ast)
                        ast))))

(def ast-passes [autoquote-lvars])

(defn tests-clj [&env & body]
  (let [[forms macroexpanded] (ana/macroexpand-all ast-passes {:locals &env} (flatten-top-level (cons 'tests body)))
        !                     (gensym "RCF__!")
        set-timeout!          (gensym "RCF__set-timeout!")
        rewritten             (->> macroexpanded
                                   (rewrite* {`hyperfiddle.rcf/!            !
                                              `hyperfiddle.rcf/set-timeout! set-timeout!})
                                   (rewrite-do)
                                   (do*->do)
                                   (simplify-do)
                                   ;; TODO *1 *2 *3 recover missing
                                   (recover-form-meta forms))]
    `(binding [*ns* (the-ns '~(symbol (str *ns*)))]
       (let [~'RCF__q          (q/queue)
             ~!                (~'fn [x#] #_(prn "!=>" x#) (q/offer! ~'RCF__q x#)) #_ (partial q/offer! ~'RCF__q)
             ~'RCF__time_start (time/current-time)
             ~'RCF__timeout    (atom hyperfiddle.rcf/*timeout*)
             ~set-timeout!     (partial reset! ~'RCF__timeout)
             ~'RCF__bindings   (binding-queue)
             ~'RCF__push!      (partial hyperfiddle.rcf/push! ~'RCF__bindings)
             ~'RCF__testing-contexts    nil]
         ~rewritten))))

(defn call-done-at-far-end [done form]
  (cond
    (empty? form)   form
    (do-form? form) (if (do-form? (last form))
                      (concat (butlast form) (list (call-done-at-far-end done (last form))))
                      (concat form (list `(~done))))
    :else           form))

(defn tests-cljs [&env & body]
  (let [async-done            (with-meta (gensym "async_done_") {:cljs.analyzer/no-resolve true})
        [forms macroexpanded] (ana/macroexpand-all ast-passes (assoc-in &env [:locals async-done] async-done)
                                                   (->> (cons 'do body)
                                                        (call-done-at-far-end async-done)
                                                        (flatten-top-level)))
        !                     (gensym "RCF__!")
        set-timeout!          (gensym "RCF__set-timeout!")
        rewritten             (->> macroexpanded
                                   (rewrite* {`hyperfiddle.rcf/!            !
                                              `hyperfiddle.rcf/set-timeout! set-timeout!})
                                   (rewrite-do)
                                   (do*->do)
                                   (simplify-do)
                                   ;; TODO *1 *2 *3 recover missing
                                   (recover-form-meta forms))]
    `(let [~'RCF__q                (q/queue)
           ~!                      (~'fn [x#] #_(prn "!=>" x#) (q/offer! ~'RCF__q x#)) #_ (partial q/offer! ~'RCF__q)
           ~'RCF__time_start       (time/current-time)
           ~'RCF__timeout          (atom hyperfiddle.rcf/*timeout*)
           ~set-timeout!           (partial reset! ~'RCF__timeout)
           ~'RCF__bindings         (binding-queue)
           ~'RCF__push!            (partial hyperfiddle.rcf/push! ~'RCF__bindings)
           ~'RCF__testing-contexts nil]
       (cljs.test/async ~async-done ~rewritten))))

(defn tests [&env & body]
  (if (ana/cljs? &env)
    (apply tests-cljs &env body)
    (apply tests-clj &env body)))

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
                                                                    (str/starts-with? cl-name "clojure.test")
                                                                    (str/starts-with? cl-name "hyperfiddle.rcf")
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
                                      :expected nil, :actual e, :ns *ns*
                                      :testing-contexts (str v)})))]
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

(defn assert-unify [m pred [_op lhs rhs :as form]]
  (let [{:keys [:message :type]} m]
    `(let [lhs#           ~lhs
           rhs#           ~rhs
           [result# env#] (unifier* lhs# rhs#)]
       (if (~pred true (u/failed? env#))
         (do (do-report {:type     ~(or type :hyperfiddle.rcf/fail), :message ~message,
                         :expected '~(recover form),      :actual  { ;;:lhs       '~(original-form lhs)
                                                                    :lhs-value lhs#
                                                                    ;;:rhs       '~(original-form rhs)
                                                                    :rhs-value rhs#
                                                                    :env       env#}
                         :ns *ns*
                         :testing-contexts ~'RCF__testing-contexts})
             lhs#)
         (do (do-report {:type     :hyperfiddle.rcf/pass, :message ~message,
                         :expected '~(recover form),      :actual  result#
                         :ns *ns*
                         :testing-contexts ~'RCF__testing-contexts})
             (if (u/failed? env#)
               lhs#
               result#))))))

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
  ([form]     `(try-expr*  nil ~form))
  ([form msg] `(try-expr* ~msg ~form)))

(defmacro testing [doc & body]
  `(let [~'RCF__testing-contexts ~doc]
     ~@body))

