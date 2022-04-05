(ns hyperfiddle.rcf
  (:require [hyperfiddle.analyzer :as ana]
            [clojure.test :as t]))

(def ^:dynamic *enabled* true)

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
                     (condp = (:var var-ast)
                       #'*1 (ana/analyze env `(peek! 0))
                       #'*2 (ana/analyze env `(peek! 1))
                       #'*3 (ana/analyze env `(peek! 2))
                       var-ast)))
   ast))

(defn rewrite-repl [env ast]
  (ana/prewalk (ana/only-nodes #{:do}
                               (fn [do-ast]
                                 (assoc do-ast :statements
                                        (loop [[s & ss] (:statements do-ast)
                                               r        []]
                                          (cond
                                            (nil? s) r
                                            (empty? ss) (recur ss (conj r s))
                                            :else
                                            (let [invoke-ast (-> (ana/analyze env `(push!))
                                                                 (update :args conj s))]
                                              (recur ss (conj r invoke-ast))))))))
               ast))


(defmulti replace-sigil identity)
(defmethod replace-sigil :default [sym] sym)
(defmethod replace-sigil := [_sym] ::=)

(defn make-is [env a b c]
  (let [sigil     (replace-sigil (:form b))
        inner-ast (-> (ana/analyze env `(~sigil))
                      (update :args conj a c))]
    (-> (make-macro-call env `(t/is))
        (update :args conj inner-ast))))

;; (def rewrite-infix nil)
(defmulti rewrite-infix (fn [_env _left center _right] (:form center)))
(defmethod rewrite-infix :default [env l c r] 
  (let [sigil-ast (ana/analyze env (replace-sigil (:form c)))]
    (make-is env l sigil-ast r)))

(defn sigil? [a] (contains? (methods t/assert-expr) (replace-sigil a)))

(defn rewrite-infix-pass [env ast]
  (ana/prewalk
   (ana/only-nodes #{:do}
                   (fn [do-ast]
                     (assoc do-ast :statements
                            (loop [ss (:statements do-ast)
                                   r []]
                              (if (>= (count ss) 3)
                                (let [[a b c] ss]
                                  (if (sigil? (:form b))
                                    (recur (drop 3 ss) (conj r (rewrite-infix env a b c)))
                                    (recur (rest ss) (conj r a))))
                                (into r ss))))))
   ast))

(defn rewrite [env ast]
  (->> ast
       (rewrite-infix-pass env)
       (rewrite-doc env)
       ;; (rewrite-star env)
       ;; (rewrite-repl env)
       ))

(defn tests* 
  ([exprs] (tests* nil exprs))
  ([env exprs]
   (when *enabled*
     (let [env (ana/to-env env)]
       (binding [ana/*emit-options* {:simplify-do true}]
         (->> (cons 'do exprs)
              (ana/analyze env)
              (rewrite env)
              (ana/emit)))))))

(defmacro tests [& body]
  (tests* (ana/to-env &env) body))

;; Nested test support
(defmethod ana/macroexpand-hook `tests [_the-var _&form _&env args] `(do ~@args))

(defmethod t/assert-expr ::= [msg form]
  (let [[_= & args] form
        form        (cons := args)]
    `(let [values# (list ~@args)
           result# (apply = values#)]
       (if result#
         (t/do-report {:type     :pass
                       :message  ~msg,
                       :expected '~form
                       :actual   (cons := values#)})
         (t/do-report {:type     :fail
                       :message  ~msg,
                       :expected '~form
                       :actual   (list '~'not (cons := values#))}))
       (first values#))))


(comment
  (ana/analyze (ana/empty-env) '(testing 1 2))
  (ana/analyze (ana/empty-env) '(do 1 2))
  (apply tests* (ana/empty-env) '("a" 1))

  (apply tests* (ana/empty-env) '(#{*1 *2 *3} := 2))
  (apply tests* (ana/empty-env) '(*1 *2 *3))
  )
