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

(defn- try-cleanup-stmts
  "Cleanup statement ASTs of a finally-only `:try` node."
  [try-node]
  (vec (mapcat :args (:finally try-node))))

(declare cps-statements)

(defn- wrap-do
  "Wrap a statement vector into a single node (for a branch / expression position)."
  [env stmts]
  (if (= 1 (count stmts))
    (first stmts)
    (assoc (ana/analyze env '(do)) :statements (vec stmts))))

(defn- cps-if
  "rcf#56: thread continuation `k` into both branches of an `:if`. `k` is bound once
   as a thunk so it runs after whichever branch's async `%` resolves — without
   duplicating `k` (which would double-count its assertions). Assumes no `%` in the
   test (ANF lifts it). Returns a one-node vector."
  [env node k]
  (if (empty? k)
    [(assoc node
       :then (wrap-do env (cps-statements env [(:then node)] []))
       :else (wrap-do env (cps-statements env [(:else node)] [])))]
    (let [ksym (gensym "RCF__k")
          call [(ana/analyze env (list ksym))]
          if'  (assoc node
                 :then (wrap-do env (cps-statements env [(:then node)] call))
                 :else (wrap-do env (cps-statements env [(:else node)] call)))]
      [(-> (ana/analyze env `(let [~ksym (fn [])]))
           (ana/resolve-syms-pass)
           (ana/macroexpand-pass)
           (update-in [:bindings 0 :init :methods 0 :body :statements] into k)
           (assoc-in [:body :statements] [if']))])))

(def ^:private ^:dynamic *recur-target*
  "Inside a threaded `:loop` body, the gensym of the self-calling step fn; `recur`
   rewrites to a call to it. nil outside a loop." nil)

(defn- recur-invoke? [ast]
  (and (= :invoke (:op ast)) (= 'recur (:form (:fn ast)))))

(defn- cps-loop
  "rcf#56: rewrite a `:loop` whose body contains `%` into a self-calling step fn —
   `recur` becomes a normal call (legal inside a poll callback) and the loop exit runs
   the continuation `k`. Completion fires once at the exit (the CPS tail), not
   per-iteration, so N iterations are not over/under-counted."
  [env node k]
  (let [params (mapv :name (:bindings node))
        inits  (mapv :init (:bindings node))
        step   (gensym "RCF__step__")
        ksym   (gensym "RCF__loopk__")
        exit   [(ana/analyze env (list ksym))]
        body   (binding [*recur-target* step]
                 (wrap-do env (cps-statements env (:statements (:body node)) exit)))]
    [(-> (ana/analyze env `(let [~ksym (fn []) ~step (fn ~step [~@params])]
                             (~step ~@(repeat (count params) nil))))
         (ana/resolve-syms-pass)
         (ana/macroexpand-pass)
         (update-in [:bindings 0 :init :methods 0 :body :statements] into k)
         (assoc-in  [:bindings 1 :init :methods 0 :body :statements] [body])
         (assoc-in  [:body :statements 0 :args] inits))]))

(defn- cps-try-finally
  "rcf#56 + exception-safety: a finally-only `:try` runs cleanup once — in the body's
   continuation after the async `%` resolves (success), or in a sync `catch` that
   reruns it and rethrows if the body throws before registering (failure). The two
   paths are mutually exclusive, so cleanup runs exactly once."
  [env node k]
  (let [cleanup  (try-cleanup-stmts node)
        cps-body (vec (cps-statements env (:statements (:body node)) (into cleanup k)))]
    [(-> (ana/analyze env `(try (do) (catch :default e# (throw e#))))
         (ana/resolve-syms-pass)
         (ana/macroexpand-pass)
         (assoc-in  [:body :statements] cps-body)
         (update-in [:catches 0 :body :statements] #(into (vec cleanup) %)))]))

(defn- anf-lift-%
  "ANF (rcf#56): replace each `%` in `ast` with a fresh temp, left-to-right, returning
   `[temps ast']` — `temps` the gensyms in poll order, `ast'` referencing them. Prunes
   user `fn`/`letfn` bodies: a `%` inside one is the frontier (it can't poll at definition
   time), so it is left untouched. A non-empty `%` remaining despite `has-%?` therefore
   means an unsequenceable `%` — the caller rejects it (no temps were produced)."
  [env ast]
  (let [!temps (atom [])
        ast'   (ana/prewalk
                 (fn [n]
                   (cond
                     (#{:fn :letfn} (:op n)) (reduced n)        ; don't lift % out of a user fn
                     (%? n) (let [t (gensym "RCF__t__")]
                              (swap! !temps conj t)
                              (-> (ana/analyze env t) (assoc :raw-forms (list (:form n)))))
                     :else n))
                 ast)]
    [@!temps ast']))

(defn- anf-lift-inits
  "Lift `%`s out of each binding `:init` (left-to-right) into temps; returns `[temps
   node']` with the inits de-`%`'d. For `:loop`, whose inits become step-fn arguments and
   so cannot carry a raw `%`."
  [env node]
  (let [!temps    (atom [])
        bindings' (mapv (fn [b]
                          (if (has-%? (:init b))
                            (let [[ts init'] (anf-lift-% env (:init b))]
                              (swap! !temps into ts)
                              (assoc b :init init'))
                            b))
                    (:bindings node))]
    [@!temps (assoc node :bindings bindings')]))

(defn- wrap-poll-binds
  "Wrap `body-node` in a `:let` binding each temp to `%` (`(let [t1 % t2 %] body)`) so
   `cps-let` polls them left-to-right before `body` runs — the ANF reduction of any
   `%`-bearing header/value to bare-`%` binding-inits. Returns the `:let` node."
  [env temps body-node]
  (-> (ana/analyze env `(let [~@(mapcat (fn [t] [t 'hyperfiddle.rcf/%]) temps)]))
      (ana/resolve-syms-pass)
      (ana/macroexpand-pass)
      (assoc-in [:body :statements] [body-node])))

(defn- cps-let
  "rcf#56 True-ANF: thread `k` through a `:let`, sequencing any `%`-bearing binding-init.
   Bindings are peeled left-to-right at the first init containing `%`:
   - bare `%` (`(let [o %] …)`) → poll-bind `(RCF__% 1 (fn [o] <rest + body>))`;
   - non-trivial `%` (`(let [o (inc %)] …)`) → ANF: lift its `%`s to bare-`%` binds
     immediately before this binding (preserving evaluation order), then bind `o` to the
     de-`%`'d init.
   The remaining bindings (recursively) and body live inside the poll callback, so
   sequential scope holds. A `:let` with no `%` in any init threads `k` through its body
   (the prior `:let` behavior).

   Term-equivalence (Geoffrey): `(let [o %] o)` ≡ `%`, making bare-`%` the canonical
   poll-bind; nested `(let [n %] (let [o n] …))` sequences identically."
  [env node k]
  (let [bindings (vec (:bindings node))
        i        (first (keep-indexed (fn [idx b] (when (has-%? (:init b)) idx)) bindings))]
    (if (nil? i)
      [(update-in node [:body :statements] #(vec (cps-statements env % k)))]
      (let [b      (nth bindings i)
            prefix (subvec bindings 0 i)
            suffix (subvec bindings (inc i))
            rest-stmts
            (if (%? (:init b))
              (let [cont (if (seq suffix)
                           (cps-let env (assoc node :bindings suffix) k)
                           (cps-statements env (:statements (:body node)) k))
                    f    (-> (ana/analyze env `(fn [~(:name b)]))
                             (ana/resolve-syms-pass)
                             (ana/macroexpand-pass)
                             (assoc-in [:methods 0 :body :statements] (vec cont)))
                    poll (-> (ana/analyze env `(~'RCF__% 1))
                             (update-in [:args] conj f))]
                [poll])
              (let [[temps init'] (anf-lift-% env (:init b))]
                (when (empty? temps)
                  (throw (ex-info "rcf#56: % inside a fn in a binding-init cannot be sequenced"
                           {:form (:form b)})))
                (cps-let env
                  (wrap-poll-binds env temps
                    (assoc node :bindings (into [(assoc b :init init')] suffix)))
                  k)))]
        (if (seq prefix)
          [(-> node (assoc :bindings prefix)
                    (assoc-in [:body :statements] (vec rest-stmts)))]
          (vec rest-stmts))))))

(defn- cps-node
  "Rewrite a %-bearing control node so continuation `k` (vector of statement
   ASTs) runs after it — including after its async body resolves. rcf#56: a
   finally-only `:try` threads its cleanup as the head of the body's
   continuation, so the async sequencer pulls cleanup into the `%` callback
   (instead of leaving it in a `finally` that fires before the body resolves).
   Returns a vector of statement ASTs."
  [env node k]
  (case (:op node)
    :do  (cps-statements env (:statements node) k)
    :let (cps-let env node k)
    :try (cond
           (seq (:catches node))   ; try/catch threading unsupported — frontier (Phase 4)
           (throw (ex-info "rcf#56: cannot sequence % across try/catch" {:form (:form node)}))
           (seq (:finally node))   ; finally-only: thread cleanup + keep sync exception-safety
           (cps-try-finally env node k)
           :else                   ; bare try (no catch/finally): just thread the body
           (cps-statements env (:statements (:body node)) k))
    :if   (if (has-%? (:test node))    ; ANF: lift the test's %s, then thread the %-free if
            (let [[temps test'] (anf-lift-% env (:test node))]
              (when (empty? temps)
                (throw (ex-info "rcf#56: % inside a fn in an if-test cannot be sequenced"
                         {:form (:form node)})))
              (cps-node env (wrap-poll-binds env temps (assoc node :test test')) k))
            (cps-if env node k))
    :loop (if (some #(has-%? (:init %)) (:bindings node))   ; ANF: lift init %s, then the loop
            (let [[temps node'] (anf-lift-inits env node)]
              (when (empty? temps)
                (throw (ex-info "rcf#56: % inside a fn in a loop binding-init cannot be sequenced"
                         {:form (:form node)})))
              (cps-node env (wrap-poll-binds env temps node') k))
            (cps-loop env node k))
    ;; rcf#56 frontier (Phase 4): post-ANF, a `%` reaching an unsupported construct (a user
    ;; `fn`/`letfn`, `recur` arg, `try/catch`) can't be sequenced — refuse to emit, rather
    ;; than leave the unbound `%` var or mis-order. A clear compile error, never silent.
    (throw (ex-info (str "rcf#56: cannot sequence % across " (:op node)
                      "; a step after the async % would run early "
                      "(supported: do/let/if/when/cond/loop/try-finally)")
             {:op (:op node), :form (:form node)}))))

(def ^:private control-flow-ops
  "Ops that sequence control rather than yield a pollable value — routed to `cps-node`
   (threaded when supported, else the frontier error). Any other %-bearing expression
   is a value whose `%`s are polled in place (ANF — covers `%` in non-assertion
   positions like `(tap (inc %))`, not just `% := …`)."
  #{:do :let :try :if :loop :letfn :fn})

(defn- cps-statements
  "Thread continuation `k` through `stmts` in CPS. A %-bearing value expression
   (assertion or arbitrary) is ANF-lifted to bare-% poll-binds and routed through
   `cps-let`; a %-bearing control-flow op is routed to `cps-node` to thread `k` through
   its tail. Returns a vector of statement ASTs."
  [env stmts k]
  (if (empty? stmts)
    (vec k)
    (let [[s & ss] stmts
          ss       (vec ss)]
      (cond
        (and *recur-target* (recur-invoke? s))   ; loop tail: recur → (step …); jump, discard k
        (if (has-%? s)
          (throw (ex-info "rcf#56: % in recur arguments is not supported" {:form (:form s)}))
          [(assoc-in s [:fn :form] *recur-target*)])

        (not (has-%? s))
        (into [s] (cps-statements env ss k))

        (control-flow-ops (:op s))
        (cps-node env s (cps-statements env ss k))

        :else   ; %-bearing value expr — ANF-lift its %s to bare-% poll-binds, then the rest
        (let [[temps s'] (anf-lift-% env s)]
          (when (empty? temps)
            (throw (ex-info "rcf#56: % cannot be sequenced from this position (inside a fn?)"
                     {:form (:form s)})))
          (cps-node env (wrap-poll-binds env temps s') (cps-statements env ss k)))))))

(defn rewrite-async-assert
  "cljs: rewrite the test body into continuation-passing style so each async `%`
   assertion's continuation = the rest of the computation, threaded through
   enclosing control-flow tails (notably `:finally`). Fixes rcf#56. clj is
   untouched — blocking `%` makes ordering irrelevant.

   The root is the `:let` injected by `maybe-add-queue-support` /
   `maybe-add-stars-support` (a `:do` only when neither wrapped the body), so
   thread the continuation through both."
  [env ast]
  (if-not (ana/cljs? env)
    ast
    ;; rcf#56 completion: seed the continuation with a single `(RCF__done!)` so it
    ;; runs at the CPS tail (once, regardless of branches/loops), replacing the
    ;; static assertion count. `add-done-support` binds RCF__done! = (once done).
    (let [k0 (if (some assertion? (ana/ast-seq ast))
               [(ana/analyze env '(RCF__done!))]
               [])]
      (case (:op ast)
        :do  (assoc ast :statements (vec (cps-statements env (:statements ast) k0)))
        :let (update-in ast [:body :statements] #(vec (cps-statements env % k0)))
        ;; `tests-cljs*` wraps the body in `(do …)`, so the root is `:do` (or the `:let`
        ;; from queue/stars support). Any other root: sequence it as a lone statement.
        (wrap-do env (cps-statements env [ast] k0))))))

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
