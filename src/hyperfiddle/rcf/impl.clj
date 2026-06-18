(ns hyperfiddle.rcf.impl
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.walk :as walk]
            [hyperfiddle.rcf.analyzer :as ana]
            [hyperfiddle.rcf.queue :as q]
            [hyperfiddle.rcf.time :as time]
            [hyperfiddle.rcf.unify :as u]
            [hyperfiddle.rcf.reporters]))

(defn rewrite-doc
  "Wrap doc-string sections in `clojure.test/testing`. A string statement followed by at
   least one statement is a doc string; it labels the statements up to the NEXT doc
   string (or block end) — sections are siblings, not nested, so a failing assertion
   reports only its own section. A string in final position is a value, not a doc (it
   has nothing to label). Consecutive doc strings yield an empty section, silently."
  [env ast]
  (ana/prewalk
   (ana/only-nodes #{:do}
                   (fn [do-ast]
                     (assoc do-ast :statements
                            (loop [[s & ss] (:statements do-ast)
                                   r []]
                              (if (nil? s) r
                                  (if (and (string? (:form s)) (seq ss))
                                    (let [ss      (vec ss)
                                          n       (count ss)
                                          ;; the next doc string closes this section; a
                                          ;; final-position string is a value, stays inside
                                          close-i (first (keep-indexed
                                                           (fn [i x]
                                                             (when (and (string? (:form x))
                                                                        (< (inc i) n))
                                                               i))
                                                           ss))
                                          section (if close-i (subvec ss 0 close-i) ss)
                                          testing-ast (ana/analyze env `(~`t/testing ~(:form s)))]
                                      (recur (when close-i (seq (subvec ss close-i)))
                                             (conj r (->> (assoc do-ast :statements section)
                                                          (update testing-ast :args conj)))))
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

(defn- scaffold-let?
  "True when `ast` is the stars-support `let` (binds `RCF__push!`, injected by
   `maybe-add-stars-support`). `rewrite-repl` must not wrap it in `(RCF__push! …)`: the
   coroutine pass (rcf#56) nests this `let` as a non-final statement of a stars-bearing
   `do`, and wrapping it would place the call outside its own binding scope (undeclared
   var). User `let`s are wrapped fine — only RCF's own push scaffolding must be skipped."
  [ast]
  (and (= :let (:op ast))
       (some #(= 'RCF__push! (:name %)) (:bindings ast))))

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
                                              (if (or (inspect-star-only? s) (scaffold-let? s))
                                                (recur ss (conj r s))
                                                (let [invoke-ast (-> (ana/analyze env '(RCF__push!))
                                                                     (update :args conj s))]
                                                  (recur ss (conj r invoke-ast))))))))))
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
                                (let [[?actual ?op ?expected] ss]
                                  (if (sigil? ?op)
                                    (recur (drop 3 ss) (conj r (rewrite-infix env ?expected ?op ?actual)))
                                    (recur (rest ss) (conj r ?actual))))
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

(defn assertion? [ast]
  (and (= :invoke (:op ast))
       (= `t/is (:form (:fn ast)))))

(defn- wrap-do
  "Wrap a statement vector into a single node (for a branch / expression position)."
  [env stmts]
  (if (= 1 (count stmts))
    (first stmts)
    (assoc (ana/analyze env '(do)) :statements (vec stmts))))

(def ^:private escaping-op?
  "Ops cloroutine compiles via `add-closing` — a break var inside one becomes an ordinary
   call that never suspends. `%` here would silently never poll, so RCF refuses it. RCF's
   analyzer renders `fn` as `:fn`; `reify`/`deftype` macroexpand to `reify*`/`deftype*`
   which it leaves as `:invoke` (caught by symbol below). A `letfn`-bound fn is an `:fn`
   `:init`, so it is covered; a `%` in a `letfn` *body* is fine (cloroutine sequences it)."
  #{:fn})

(def ^:private escaping-invoke-syms
  "Macroexpanded escaping constructs RCF's analyzer leaves as `:invoke` (no `-parse`)."
  '#{reify* deftype* proxy})

(defn- escaping-node? [n]
  (or (escaping-op? (:op n))
      (and (= :invoke (:op n))
           (let [f (:form (:fn n))]
             (and (symbol? f) (escaping-invoke-syms (symbol (name f))))))))

(defn- assert-no-%-in-fns!
  "rcf#56 frontier: refuse `%` inside an escaping fn body (`fn`, a `letfn`/`reify`/`deftype`
   bound fn). cloroutine treats a break var inside an escaping fn as an ordinary call (it
   never suspends there), so such a `%` would silently never poll — we throw a clear
   compile error instead. A `%` in a `letfn` *body* is fine and is not refused here."
  [ast]
  (when-let [n (first (filter (fn [n] (and (escaping-node? n) (has-%? n)))
                              (ana/ast-seq ast)))]
    (throw (ex-info "rcf#56: % inside a fn cannot be sequenced — it would never poll (cloroutine ignores breaks in escaping fns)"
                    {:op (:op n) :form (:form n)}))))

(defn- rewrite-%->park
  "Replace each `%` (a `hyperfiddle.rcf/%` var ref) with a `(park-tap)` call — cloroutine's
   suspension site. Assumes `assert-no-%-in-fns!` already passed, so every remaining `%`
   sits where cloroutine can sequence it (do/let/if/cond/loop/recur/try)."
  [env ast]
  (ana/postwalk
   (ana/only-nodes %?
                   (fn [var-ast]
                     (-> (ana/analyze env `(hyperfiddle.rcf/park-tap))
                         (assoc :raw-forms (list (:form var-ast))))))
   ast))

(defn- wrap-coroutine
  "Wrap test-body `stmts` (already %→park rewritten) in a cloroutine driven by the async
   queue. Emits, verbatim — RCF does NOT macroexpand cloroutine; the cljs compiler does:

     (drive-tap RCF__% (cloroutine.core/cr {park-tap unpark-tap} (do stmts… [(RCF__done!)])))

   `RCF__%` (the make-queue poll closure, in scope from `maybe-add-queue-support`) feeds
   the resume var; `(RCF__done!)` is the completing tail (only when assertions exist —
   `add-done-support` binds it then). Returns a one-statement vector."
  [env stmts done?]
  (let [body  (cond-> (vec stmts) done? (conj (ana/analyze env `(~'RCF__done!))))
        shell (ana/analyze env `(hyperfiddle.rcf/drive-tap ~'RCF__%
                                  (cloroutine.core/cr {hyperfiddle.rcf/park-tap hyperfiddle.rcf/unpark-tap}
                                    (do))))]
    [(assoc-in shell [:args 1 :args 1 :statements] body)]))

(defn rewrite-async-assert
  "cljs: drive the test body as a cloroutine (rcf#56). Each `%` becomes a `(park-tap)`
   suspension; `cloroutine.core/cr` sequences control flow across it — if/cond/loop/recur
   and try (including catch, which the in-house CPS refused); `drive-tap` feeds the async
   queue's values back in; `(RCF__done!)` completes once at the tail. clj is untouched —
   blocking `%` makes ordering irrelevant.

   The root is the `:let` injected by `maybe-add-queue-support` / `maybe-add-stars-support`
   (a `:do` only when neither wrapped the body — i.e. no `%`)."
  [env ast]
  (if-not (ana/cljs? env)
    ast
    (let [has-%      (has-%? ast)
          has-assert (some assertion? (ana/ast-seq ast))]
      (when has-% (assert-no-%-in-fns! ast))
      (let [ast      (if has-% (rewrite-%->park env ast) ast)
            seq-tail (fn [stmts]
                       (cond
                         has-%      (wrap-coroutine env stmts has-assert)  ; coroutine + done tail
                         has-assert (conj (vec stmts) (ana/analyze env `(~'RCF__done!)))
                         :else      (vec stmts)))]
        (case (:op ast)
          :do  (assoc ast :statements (seq-tail (:statements ast)))
          :let (update-in ast [:body :statements] seq-tail)
          (wrap-do env (seq-tail [ast])))))))

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
            body (-> (ana/analyze env `(let [~'RCF__done! (hyperfiddle.rcf/once ~done-sym)]))
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

(defn- case->cond
  "Expand `(case e clause...)` to `(let [v e] (cond ...))` so the cljs CPS rewrite
   sequences case branches via the `if` rule. Preserves test order; drops case's
   constant-time dispatch (irrelevant for tests). No-match without a default yields nil
   (case would throw) — acceptable in test bodies."
  [args]
  (let [[e & clauses] args
        v        (gensym "case__")
        default? (odd? (count clauses))
        default  (when default? (last clauses))
        pairs    (partition 2 (if default? (butlast clauses) clauses))
        branches (mapcat (fn [[test result]]
                           [(if (seq? test)
                              `(contains? (quote ~(set test)) ~v)
                              `(= ~v (quote ~test)))
                            result])
                   pairs)]
    `(let [~v ~e]
       (cond ~@branches ~@(when default? [:else default])))))

;; Nested test support
(defmethod ana/macroexpand-hook `hyperfiddle.rcf/tests [_the-var _&form _&env args] `(do ~@args))
;; clojure.test/is support
(defmethod ana/macroexpand-hook `t/is [_the-var _&form _&env args] `(t/is ~@args))

;; Skip these DSLs, their macroexpansion is not rewritable as clojure. 
(defmethod ana/macroexpand-hook 'clojure.core/case [_ _ _ args] `(case ~@args)) ; clj: opaque (blocking, unchanged)
(defmethod ana/macroexpand-hook 'cljs.core/case [_ _ _ args] (case->cond args)) ; cljs: expand for CPS
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

(defn- stacktrace-file-and-line
  [stacktrace]
  (if (seq stacktrace)
    (let [^StackTraceElement s (first stacktrace)
          file-name (.getFileName s)
          file-name (if (= "NO_SOURCE_FILE" file-name) (str (ns-name *ns*)) file-name)]
      {:file file-name :line (.getLineNumber s)})
    {:file nil :line nil}))


(defn do-report* [m]
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

(defmacro do-report [m]
  (if (:js-globals &env)
    ;; mirror do-report* below: cljs.test/do-report stitches file/line only for the
    ;; standard :fail/:error types, so rcf-typed fails arrive without source location —
    ;; stitch from the JS stack here, at the assertion site (depth 1 skips the V8
    ;; "Error" header line). Explicit keys in m win (merge is right-biased).
    `(let [m# ~m]
       (cljs.test/do-report
         (if (= :hyperfiddle.rcf/fail (:type m#))
           (merge (cljs.test/file-and-line (js/Error.) 1) m#)
           m#)))
    `(do-report* ~m)))

(defmethod t/assert-expr 'hyperfiddle.rcf/thrown? [msg form]
  ;; (is (thrown? c expr))
  ;; Asserts that evaluating expr throws an exception of class c.
  ;; Returns the exception thrown.
  (let [[klass body] (rest form)]
    `(try ~body
          (do-report {:type :hyperfiddle.rcf/fail, :message ~msg,
                      :expected '~form, :actual nil})
          (catch ~klass e#
            (do-report {:type :hyperfiddle.rcf/pass, :message ~msg,
                        :expected '~form, :actual e#})
            e#))))

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
