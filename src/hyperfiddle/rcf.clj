(ns hyperfiddle.rcf
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.walk :as walk]
            [hyperfiddle.analyzer :as ana]
            [hyperfiddle.rcf.queue :as q]
            [hyperfiddle.rcf.time :as time]
            [hyperfiddle.rcf.unify :as u]
            [hyperfiddle.rcf.reporters]))

(def ^:dynamic *enabled* true)

(def ! #(doto % prn))
(def %)

(defn make-macro-call [env form]
  (binding [ana/*analyze-options* (assoc ana/*analyze-options* :macroexpand false)]
    (ana/analyze env form)))

(defn rewrite-doc [env ast]
  (ana/prewalk
   (ana/only-nodes #{:do}
                   (fn [do-ast]
                     (assoc do-ast :statements
                            (loop [[s & ss] (:statements do-ast)
                                   r []]
                              (if (nil? s) r
                                  (if (string? (:form s))
                                    (let [testing-ast (make-macro-call env `(~`t/testing ~(:form s)))]
                                      (->> (assoc do-ast :statements (vec ss))
                                           (update testing-ast :args conj)
                                           (conj r)))
                                    (recur ss (conj r s))))))))
   ast))

(defn rewrite-star [env ast]
  (ana/postwalk
   (ana/only-nodes #{:var}
                   (fn [var-ast]
                     (if-let [index (condp = (:var var-ast) #'*1 0, #'*2, 1 #'*3 2, nil)]
                       (-> (ana/analyze env `(~'RCF__peek! ~index))
                           (assoc :raw-forms (list (:form var-ast)))
                           (assoc ::star true))
                       var-ast)))
   ast))

(defn- push-binding [q d] (let [[c b _a] q] [d c b]))

(defn binding-queue []
  (let [!q    (atom [nil nil nil])
        push! (partial swap! !q push-binding)
        peek! #(get (deref !q) %)]
    [push! peek!]))

(defn star? [ast] (or (::star ast)
                      (and (= :var (:op ast))
                           (#{#'*1 #'*2 #'*3} (:var ast)))))

(defn has-stars? [ast] (some? (first (filter star? (ana/ast-seq ast)))))

(defn maybe-add-stars-support [env ast]
  (if (has-stars? ast)
    (-> (ana/analyze env `(let [[~'RCF__push! ~'RCF__peek!] (binding-queue)]))
        (update-in [:body :statements] conj ast))
    ast))


(def ^:dynamic *timeout* 400)

(defmacro make-queue [timeout-value]
  `(let [q#       (q/queue)
         start#   (time/current-time)
         timeout# (atom *timeout*)]
     [(fn [x#] (q/offer! q# x#) x#)
      (fn [] (q/poll! q# start# (deref timeout#) ~timeout-value))
      (partial reset! timeout#)]))

(defn %? [ast] (and (= :var (:op ast)) (= #'% (:var ast))))
(defn has-%? [ast] (some? (first (filter %? (ana/ast-seq ast)))))
(defn maybe-add-queue-support [env ast]
  (if (has-%? ast)
    (-> (ana/analyze env `(let [[~'RCF__! ~'RCF__% ~'RCF__set-timeout!] (make-queue ::timeout)]))
        (update-in [:body :statements] conj ast))
    ast))

(defn rewrite-!-% [env ast]
  (if-not (has-%? ast)
    ast
    (ana/postwalk
     (ana/only-nodes #{:var}
                     (fn [var-ast]
                       (condp = (:var var-ast)
                         #'! (assoc var-ast :form 'RCF__!)
                         #'% (-> (ana/analyze env `(~'RCF__%))
                                 (assoc :raw-forms (list (:form var-ast))))
                         var-ast)))
     ast)))

(defn rewrite-repl [env ast]
  (ana/prewalk (ana/only-nodes #{:do}
                               (fn [do-ast]
                                 (if-not (has-stars? do-ast)
                                   do-ast
                                   (assoc do-ast :statements
                                          (loop [[s & ss] (:statements do-ast)
                                                 r        []]
                                            (cond
                                              (nil? s) r
                                              (empty? ss) (recur ss (conj r s))
                                              :else
                                              (let [invoke-ast (-> (ana/analyze env '(RCF__push!))
                                                                   (update :args conj s))]
                                                (recur ss (conj r invoke-ast)))))))))
               ast))


(defmulti replace-sigil identity)
(defmethod replace-sigil :default [sym] sym)
(defmethod replace-sigil := [_sym] ::=)
(defmethod replace-sigil '= [_sym] 'hyperfiddle.rcf/=)

(defn make-is [env a b c]
  (let [sigil     (replace-sigil (:form b))
        inner-ast (-> (ana/analyze env `(~sigil))
                      (update :args conj a c))]
    (-> (make-macro-call env `(t/is))
        (update :args conj inner-ast))))

(defn lvar? [ast]
  (and (#{:var :symbol} (:op ast))
       (or (= '_ (:form ast))
           (str/starts-with? (str (:form ast)) "?"))))
(defn has-lvars? [ast] (some? (first (filter lvar? (ana/ast-seq ast)))))

(defn simplify-sigil [left center right]
  (cond
    (and (= :var (:op center))
         (= 'hyperfiddle.rcf/= (ana/var-sym (:var center)))) '=
    (and (= := (:form center))
         (not (has-lvars? left))
         (not (has-lvars? right))) '=
    :else (:form center)))

(defmulti rewrite-infix (fn [_env _left center _right] (:form center)))
(defmethod rewrite-infix :default [env l c r]
  (let [sigil-ast  (ana/analyze env (replace-sigil (simplify-sigil l c r)))]
    (make-is env l sigil-ast r)))

(defn sigil? [ast]
  (let [form (if (= :var (:op ast))
               (ana/var-sym (:var ast))
               (:form ast))]
    (contains? (methods t/assert-expr) (replace-sigil form))))

(defn rewrite-infix-pass [env ast]
  (ana/prewalk
   (ana/only-nodes #{:do}
                   (fn [do-ast]
                     (assoc do-ast :statements
                            (loop [ss (:statements do-ast)
                                   r []]
                              (if (>= (count ss) 3)
                                (let [[a b c] ss]
                                  (if (sigil? b)
                                    (recur (drop 3 ss) (conj r (rewrite-infix env a b c)))
                                    (recur (rest ss) (conj r a))))
                                (into r ss))))))
   ast))

(defn autoquote-lvars [env ast]
  (ana/postwalk
   (ana/only-nodes #{:var :symbol}
                   (fn [ast]
                     (if (lvar? ast)
                       (-> (update ast :form #(list 'quote %))
                           (update :raw-forms (fnil conj ()) (:form ast)))
                       ast)))
   ast))

(defn rewrite [env ast]
  (->> ast
       (maybe-add-stars-support env)
       (maybe-add-queue-support env)
       (rewrite-!-% env)
       (rewrite-infix-pass env)
       (rewrite-doc env)
       (rewrite-star env)
       (rewrite-repl env)
       (autoquote-lvars env)))

(defn tests*
  ([exprs] (tests* nil exprs))
  ([env exprs]
   (let [env (ana/to-env env)]
     (binding [ana/*emit-options* {:simplify-do true}]
       (->> (cons 'do exprs)
            (ana/analyze env)
            (rewrite env)
            (ana/emit))))))

(defn gen-name [form]
  (let [{:keys [line _column]} (meta form)
        file (name (ns-name *ns*))]
    (symbol (str file ":" line))))

(defmacro tests [& body]
  (cond
    *generate-tests* `(deftest ~(gen-name &form) ~(tests* &env body))
    *enabled*        (tests* &env body)
    :else             nil))

;; Nested test support
(defmethod ana/macroexpand-hook `tests [_the-var _&form _&env args] `(do ~@args))

(defn original-form [form]
  (walk/prewalk (fn [form]
                  (if (ana/has-meta? form)
                    (or (first (::ana/macroexpanded (meta form)))
                        form)
                    form))
                form))

;; We can use '= to prevent unification (ignore wildcards and ?vars)
;; To avoid conflicts with cc/=, RCF symbolicaly desugares cc/= to rcf/=.
;; So rcf/= need a var to resolve to.
(def = clojure.core/=)

;; Same as default `=` behavior, but returns the first argument instead of a boolean.
(defmethod t/assert-expr 'hyperfiddle.rcf/= [msg form]
  (let [[_= & args] form
        form        (cons '= (map original-form args))]
    `(let [values# (list ~@args)
           result# (apply = values#)]
       (if result#
         (t/do-report {:type     ::pass
                       :message  ~msg,
                       :expected '~form
                       :actual   (cons '= values#)})
         (t/do-report {:type     ::fail
                       :message  ~msg,
                       :expected '~form
                       :actual   (list '~'not (cons '~'= values#))}))
       (first values#))))

(defmethod t/assert-expr ::= [msg form]
  (let [[_= & args] form
        form        (cons := (map original-form args))]
    `(let [lhs#           (identity ~(first args))
           rhs#           (identity ~(second args))
           [result# env#] (u/unifier* lhs# rhs#)]
       (if-not (u/failed? env#)
         (do (t/do-report {:type     ::pass
                           :message  ~msg,
                           :expected '~form
                           :actual   result#})
             result#)
         (do (t/do-report {:type     ::fail
                           :message  ~msg,
                           :expected '~form
                           :actual   env#})
             lhs#)))))



(defn test-var
  "Like `clojure.test/test-var` but return actual result."
  [v]
  (when-let [t (:test (meta v))]
    (binding [t/*testing-vars* (conj t/*testing-vars* v)]
      (t/do-report {:type :begin-test-var, :var v})
      (t/inc-report-counter :test)
      (try (t)
           (catch Throwable e
             (t/do-report {:type :error, :message "Uncaught exception, not in assertion."
                           :expected nil, :actual e}))
           (finally (t/do-report {:type :end-test-var, :var v}))))))

(defmacro deftest
  "When *load-tests* is false, deftest is ignored."
  {:added "1.1"}
  [name & body]
  (when t/*load-tests*
    `(def ~(vary-meta name assoc :test `(fn [] ~@body))
       (fn [] (test-var (var ~name))))))
(comment
  (ana/analyze (ana/empty-env) '(testing 1 2))
  (ana/analyze (ana/empty-env) '(do 1 2))
  (apply tests* (ana/empty-env) '("a" 1))

  (apply tests* (ana/empty-env) '(#{*1 *2 *3} := 2))
  (apply tests* (ana/empty-env) '(*1 *2 *3))
  )
