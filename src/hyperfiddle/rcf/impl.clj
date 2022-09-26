(ns hyperfiddle.rcf.impl
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.walk :as walk]
            [hyperfiddle.rcf.analyzer :as ana]
            [hyperfiddle.rcf.queue :as q]
            [hyperfiddle.rcf.time :as time]
            [hyperfiddle.rcf.unify :as u]
            [hyperfiddle.rcf.reporters]))

(defn rewrite-doc [env ast]
  (ana/prewalk
   (ana/only-nodes #{:do}
                   (fn [do-ast]
                     (assoc do-ast :statements
                            (loop [[s & ss] (:statements do-ast)
                                   r []]
                              (if (nil? s) r
                                  (if (and (string? (:form s)) (seq ss))
                                    (let [testing-ast (ana/analyze env `(~`t/testing ~(:form s)))]
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


(defn star? [ast] (or (::star ast)
                      (and (= :var (:op ast))
                           (#{#'*1 #'*2 #'*3} (:var ast)))))

(defn has-stars? [ast] (some? (first (filter star? (ana/ast-seq ast)))))

(defn maybe-add-stars-support [env ast]
  (if (has-stars? ast)
    (-> (ana/analyze env `(let [[~'RCF__push! ~'RCF__peek!] (hyperfiddle.rcf/binding-queue)]))
        (ana/resolve-syms-pass)
        (ana/macroexpand-pass)
        (update-in [:body :statements] conj ast))
    ast))

(defn make-queue [timeout-value]
  `(let [q#       (q/queue)
         start#   (time/current-time)
         timeout# (atom hyperfiddle.rcf/*timeout*)]
     [(fn [x#] (q/offer! q# x#) x#)
      (fn
        ([] (q/poll! q# start# (deref timeout#) ~timeout-value))          ; blocking
        ([n# cb#] (q/poll-n! q# start# (deref timeout#) ~timeout-value n# cb#)) ; non blocking
        )
      (partial reset! timeout#)]))

(defn %? [ast] (and (= :var (:op ast)) (= 'hyperfiddle.rcf/% (ana/var-sym (:var ast)))))
(defn has-%? [ast] (some? (first (filter %? (ana/ast-seq ast)))))
(defn maybe-add-queue-support [env ast]
  (if (has-%? ast)
    (-> (ana/analyze env `(let [[~'RCF__tap ~'RCF__% ~'RCF__set-timeout!] (hyperfiddle.rcf/make-queue :hyperfiddle.rcf/timeout)]))
        (ana/resolve-syms-pass)
        (ana/macroexpand-pass)
        (update-in [:body :statements] conj ast))
    ast))

(defn rewrite-tap-% [env ast]
  (if-not (has-%? ast)
    ast
    (ana/postwalk
     (ana/only-nodes #{:var}
                     (fn [var-ast]
                       (condp = (ana/var-sym (:var var-ast))
                         'hyperfiddle.rcf/tap (assoc var-ast :form 'RCF__tap)
                         'hyperfiddle.rcf/! (assoc var-ast :form 'RCF__tap)
                         'hyperfiddle.rcf/set-timeout! (assoc var-ast :form 'RCF__set-timeout!)
                         'hyperfiddle.rcf/%  (if (ana/cljs? env)
                                               var-ast
                                               (-> (ana/analyze env `(~'RCF__%))
                                                   (assoc :raw-forms (list (:form var-ast)))))
                         var-ast)))
     ast)))

(defn- inspect-star-only? [ast]
  (and (= `t/is (:form (:fn ast)))
       (let [[left right] (some-> ast :args first :args)]
         (or (and (star? left) (not (has-stars? right)))
             (and (star? right) (not (has-stars? left)))))))

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
                                              (if-not (inspect-star-only? s)
                                                (let [invoke-ast (-> (ana/analyze env '(RCF__push!))
                                                                     (update :args conj s))]
                                                  (recur ss (conj r invoke-ast)))
                                                (recur ss (conj r s)))))))))
               ast))


(defmulti replace-sigil identity)
(defmethod replace-sigil :default [sym] sym)
(defmethod replace-sigil := [_sym] :hyperfiddle.rcf/=)
(defmethod replace-sigil 'thrown? [_sym] 'hyperfiddle.rcf/thrown?)
(defmethod replace-sigil :throws [_sym] 'hyperfiddle.rcf/thrown?)

(defn replace-sigil* [sym]
  (let [sym' (replace-sigil sym)]
    (if (= sym sym')
      sym' (recur sym'))))

(defn make-is [env a b c]
  (let [sigil     (replace-sigil* (:form b))
        inner-ast (-> (ana/analyze env `(~sigil))
                      (update :args conj a c))]
    (-> (ana/analyze env `(t/is))
        (update :args conj inner-ast))))

(defn lvar? [ast]
  (and (#{:var :symbol} (:op ast))
       (or (= '_ (:form ast))
           (str/starts-with? (str (:form ast)) "?"))))
(defn has-lvars? [ast] (some? (first (filter lvar? (ana/ast-seq ast)))))

(defn simplify-sigil [left center right]
  (cond
    (and (= := (:form center))
         (not (has-lvars? left))
         (not (has-lvars? right))) 'hyperfiddle.rcf/=
    :else (:form center)))

(defmulti rewrite-infix (fn [_env _left center _right] (:form center)))
(defmethod rewrite-infix :default [env l c r]
  (let [sigil-ast  (ana/analyze env (replace-sigil* (simplify-sigil l c r)))]
    (make-is env l sigil-ast r)))

(defn sigil? [ast]
  (let [methods (methods t/assert-expr)
        sigil   (replace-sigil* (if (= :var (:op ast))
                                  (ana/var-sym (:var ast))
                                  (:form ast)))]
    (or (contains? methods sigil)
        (when (or (keyword? sigil) (symbol? sigil))
          (contains? methods (symbol (name sigil)))))))

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
  ;; Rewrites ?a and _ in t/is assertions
  ;; It could be handled by t/is directly, but we already have a full AST here, 
  ;; no need to serialize it only to reparse it all on the next macroexpand.
  (ana/prewalk
   (ana/only-nodes #{:invoke}
                   (fn [ast]
                     (if-not (and (= `t/is (:form (:fn ast)))
                                  (sigil? (-> ast :args first :fn))                                  )
                       ast
                       (ana/postwalk
                        (ana/only-nodes #{:var :symbol}
                                        (fn [ast]
                                          (if (lvar? ast)
                                            (-> (update ast :form #(list 'quote %))
                                                (update :raw-forms (fnil conj ()) (:form ast)))
                                            ast))) ast)))) ast))

(defn rewrite-is-support [env ast]
  (ana/prewalk
   (ana/only-nodes #{:invoke}
                   (fn [ast]
                     (if-not (= `t/is (:form (:fn ast)))
                       ast
                       (update-in ast [:args 0 :fn :form] replace-sigil*)))) ast))

(defn rewrite-cljs-test [env ast]
  (if-not (ana/cljs? env)
    ast
    (ana/prewalk
     (ana/only-nodes #{:var :symbol}
                     (fn [ast]
                       (if (and (symbol? (:form ast))
                                (= "clojure.test" (namespace (:form ast))))
                         (assoc ast :form (symbol "cljs.test" (name (:form ast))))
                         ast))) ast)))

(defn make-poll-n [n env ast]
  (let [syms (take n (map #(symbol (str "%-" (inc %))) (range n)))
        ast  (let [!syms (atom syms)]
               (ana/postwalk (ana/only-nodes #{:var}
                                             (fn [var-ast]
                                               (if (empty? @!syms)
                                                 (reduced var-ast)
                                                 (condp = (ana/var-sym (:var var-ast))
                                                   'hyperfiddle.rcf/% (let [sym (first @!syms)]
                                                                        (swap! !syms rest)
                                                                        (-> (ana/analyze env sym)
                                                                            (assoc :raw-forms (list (:form var-ast)))))
                                                   var-ast))))
                             ast))
        f    (-> (ana/analyze env `(fn [~@syms]))
                 (ana/resolve-syms-pass)
                 (ana/macroexpand-pass)
                 (update-in [:methods 0 :body :statements] conj ast))]
    (-> (ana/analyze env `(~'RCF__% ~n))
        (update-in [:args] conj f))))


(defn assertion? [ast]
  (and (= :invoke (:op ast))
       (= `t/is (:form (:fn ast)))))

(defn rewrite-async-assert [env ast]
  (if-not (ana/cljs? env)
    ast
    (ana/prewalk
     (ana/only-nodes #{:do}
                     (fn [ast]
                       (assoc ast :statements
                              (loop [r        []
                                     [s & ss] (:statements ast)]
                                (cond
                                  (nil? s)             r
                                  (not (assertion? s)) (recur (conj r s) ss)
                                  :else (let [%-count (count (filter %? (ana/ast-seq s)))]
                                          (if (zero? %-count)
                                            (recur (conj r s) ss)
                                            (let [s' (-> (ana/analyze (ana/empty-env) '(do))
                                                         (update :statements into (cons s ss)))]
                                              (conj r (make-poll-n %-count env s')))))))))) ast)))

(defn rewrite-overload-is [env ast]
  (ana/prewalk
   (ana/only-nodes #{:invoke}
                   (fn [ast]
                     (if (#{`t/is 'cljs.test/is} (:form (:fn ast)))
                       (assoc-in ast [:fn :form] 'hyperfiddle.rcf/is)
                       ast))) ast))

(defn add-done-support [env ast]
  (let [count-is  (count (filter (fn [ast] (and (= :invoke (:op ast)) (= `t/is (:form (:fn ast))))) (ana/ast-seq ast)))]
    (if (zero? count-is)
      ast
      (let [done-sym (gensym "done-")
            body (-> (ana/analyze env `(let [~'RCF__done! (hyperfiddle.rcf/async-notifier ~count-is ~done-sym)]))
                     (ana/resolve-syms-pass)
                     (ana/macroexpand-pass)
                     (update-in [:body :statements] conj ast))]
        (-> (ana/analyze env `(~'hyperfiddle.rcf/async ~done-sym))
            (update-in [:args] conj body))))))

(defn rewrite [env ast]
  (->> ast
       (maybe-add-stars-support env)
       (maybe-add-queue-support env)
       (rewrite-tap-% env)
       (rewrite-infix-pass env)
       (rewrite-async-assert env)
       (rewrite-doc env)
       (rewrite-star env)
       (rewrite-repl env)
       (autoquote-lvars env)
       (rewrite-is-support env)
       (add-done-support env)
       (rewrite-cljs-test env)
       (rewrite-overload-is env)))

(defn tests-cljs* [env exprs]
  (let [env (ana/to-env env)]
    (binding [ana/*emit-options* {:simplify-do true}]
      (->> (cons 'do exprs)
           (ana/analyze env)
           (ana/resolve-syms-pass)
           (ana/macroexpand-pass)
           (rewrite env)
           (ana/emit)))))

(defn tests*
  ([exprs] (tests* nil exprs))
  ([env exprs]
   (if (ana/cljs? env)
     (tests-cljs* env exprs)
     `(binding [*ns* ~*ns*]
        ~(let [env (ana/to-env env)]
           (binding [ana/*emit-options* {:simplify-do true}]
             (->> (cons 'do exprs)
                  (ana/analyze env)
                  (ana/resolve-syms-pass)
                  (ana/macroexpand-pass)
                  (rewrite env)
                  (ana/emit))))))))

;; Nested test support
(defmethod ana/macroexpand-hook `hyperfiddle.rcf/tests [_the-var _&form _&env args] `(do ~@args))
;; clojure.test/is support
(defmethod ana/macroexpand-hook `t/is [_the-var _&form _&env args] `(t/is ~@args))

;; Skip these DSLs, their macroexpansion is not rewritable as clojure. 
(defmethod ana/macroexpand-hook 'clojure.core/case [_ _ _ args] `(case ~@args))
(defmethod ana/macroexpand-hook 'cljs.core/case [_ _ _ args] `(case ~@args))
(defmethod ana/macroexpand-hook 'clojure.core.async/go [_ _ _ args] (reduced `(clojure.core.async/go (do ~@args))))
(defmethod ana/macroexpand-hook 'cljs.core.async/go [_ _ _ args] (reduced `(cljs.core.async/go (do ~@args))))

(defn quoted? [form] (and (seq? form) (= 'quote (first form))))

(defn original-form [form]
  (walk/prewalk (fn [form]
                  (if (ana/has-meta? form)
                    (if-some [form (::ana/macroexpanded (meta form))]
                      (if (quoted? form) (second form) form)
                      form)
                    form))
                form))

;; Tag asserted forms with the original user input (form before macroexpand)
;; so t/is can report it as typed by user, not as rewritten by RCF.
(defmethod ana/-emit :invoke [ast]
  (if (sigil? (:fn ast))
    (list* (ana/emit (:fn ast)) (map (fn [arg] (let [form (ana/emit arg)]
                                                 (if-some [original-form (some-> arg :raw-forms seq last)]
                                                   (with-meta form {::ana/macroexpanded original-form})
                                                   form))) (:args ast)))
    (ana/emit-invoke ast)))

(defmethod t/assert-expr 'hyperfiddle.rcf/thrown? [msg form]
  ;; (is (thrown? c expr))
  ;; Asserts that evaluating expr throws an exception of class c.
  ;; Returns the exception thrown.
  (let [[body klass] (rest form)]
    `(try ~body
          (do-report {:type :hyperfiddle.rcf/fail, :message ~msg,
                      :expected '~form, :actual nil})
          (catch ~klass e#
            (do-report {:type :hyperfiddle.rcf/pass, :message ~msg,
                        :expected '~form, :actual e#})
            e#))))


(defn- stacktrace-file-and-line
  [stacktrace]
  (if (seq stacktrace)
    (let [^StackTraceElement s (first stacktrace)
          file-name (.getFileName s)
          file-name (if (= "NO_SOURCE_FILE" file-name) (str (ns-name *ns*)) file-name)]
      {:file file-name :line (.getLineNumber s)})
    {:file nil :line nil}))

(defn do-report [m]
  (t/report
   (case
    (:type m)
     (:fail :hyperfiddle.rcf/fail) (merge (stacktrace-file-and-line (drop-while
                                                                     #(let [cl-name (.getClassName ^StackTraceElement %)]
                                                                        (or (str/starts-with? cl-name "java.lang.")
                                                                            (str/starts-with? cl-name "clojure.test$")
                                                                            (str/starts-with? cl-name "clojure.core$ex_info")
                                                                            (str/starts-with? cl-name "hyperfiddle.rcf")))
                                                                     (.getStackTrace (Thread/currentThread)))) m)
     (:error :hyperfiddle.rcf/error) (merge (stacktrace-file-and-line (.getStackTrace ^Throwable (:actual m))) m)
     m)))

(defn test-var
  "Like `clojure.test/test-var` but return actual result."
  [v]
  (when-let [t (:test (meta v))]
    (binding [t/*testing-vars* (conj t/*testing-vars* v)]
      (do-report {:type :begin-test-var, :var v})
      (t/inc-report-counter :test)
      (try (t)
           (catch Throwable e
             (do-report {:type :error, :message "Uncaught exception, not in assertion."
                         :expected nil, :actual e}))
           (finally (do-report {:type :end-test-var, :var v}))))))
