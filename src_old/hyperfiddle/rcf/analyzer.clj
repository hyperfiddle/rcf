(ns hyperfiddle.rcf.analyzer
  (:require [cljs.analyzer :as a]
            [cljs.analyzer.api :as aa]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as jvm]
            [clojure.tools.analyzer.passes.emit-form :as default]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form])
  (:import (clojure.lang Compiler$LocalBinding)
           #_(clojure.lang Var)))

(defn cljs? [env] (some? (:js-globals env)))

(declare emit)

(defn emit-method [{:keys [variadic? params body]}]
  (list (if variadic?
          (-> (into [] (map :name) (pop params))
              (conj '& (-> params peek :name)))
          (into [] (map :name) params)) (emit body)))

(defn emit [ast]
  (case (:op ast)
    :let        (list 'let (into [] (mapcat
                                     (fn [{:keys [name init]}]
                                       [name (emit init)]))
                                 (:bindings ast))
                      (emit (:body ast)))
    :loop       (list 'loop (into [] (mapcat
                                      (fn [{:keys [name init]}]
                                        [name (emit init)]))
                                  (:bindings ast))
                      (emit (:body ast)))
    :recur      (cons 'recur (map emit (:exprs ast)))
    :invoke     (map emit (cons (:fn ast) (:args ast)))
    :fn         (cons 'fn (concat (when-some [l (:local ast)] [(:name l)])
                                  (map emit-method (:methods ast))))
    :letfn      (list 'letfn (into [] (map (fn [{:keys [name init]}]
                                             (cons name (map emit-method (:methods init)))))
                                   (:bindings ast))
                      (emit (:body ast)))
    :try        (list* 'try (emit (:body ast))
                       (list 'catch :default (:name ast) (emit (:catch ast)))
                       (when-some [f (:finally ast)]
                         [(list 'finally (emit f))]))
    :throw      (list 'throw (emit (:exception ast)))
    :new        (cons 'new (map emit (cons (:class ast) (:args ast))))
    :def        (list 'def (emit (:var ast)) (emit (:init ast)))
    :set!       (list 'set! (emit (:target ast)) (emit (:val ast)))
    :js         (list* 'js* (or (:code ast) (apply str (interpose "~{}" (:segs ast)))) (map emit (:args ast)))
    :do         (cons 'do (conj (mapv emit (:statements ast)) (emit (:ret ast))))
    :map        (zipmap (map emit (:keys ast)) (map emit (:vals ast)))
    :set        (into #{} (map emit) (:items ast))
    :vec        (into [] (map emit) (:items ast))
    :if         (list 'if (emit (:test ast)) (emit (:then ast)) (emit (:else ast)))
    :case       (list* 'case (emit (:test ast))
                       (-> []
                           (into (mapcat (fn [{:keys [tests then]}]
                                           [(map :form tests) (emit (:then then))]))
                                 (:nodes ast))
                           (conj (emit (:default ast)))))
    :host-field (list '. (emit (:target ast)) (symbol (str "-" (name (:field ast)))))
    :host-call  (list* '. (emit (:target ast)) (:method ast) (map emit (:args ast)))
    :var        (:name ast)
    ::lvar      (list 'quote (:form ast))
    :quote      (list 'quote (emit (:expr ast)))
    ;; else
    (:form ast)))

(def macroexpand-1* @#'jvm/macroexpand-1)

(def emit-form* emit-form/emit-form)

(defn coerce-local [[symbol binding]]
  [symbol (or (when (instance? Compiler$LocalBinding binding)
                (let [binding ^Compiler$LocalBinding binding]
                  {:op   :local
                   :tag  (when (.hasJavaClass binding)
                           (some-> binding (.getJavaClass)))
                   :form symbol
                   :name symbol}))
              binding)])

(defn apply-passes [passes env ast]
  (reduce (fn [ast pass] (pass env ast)) ast passes))

(defn ensure-ns [ns]
  (when (some? ns)
    (create-ns ns)
    ns))

#_(defn create-var
  "Creates a Var for sym and returns it.
   The Var gets interned in the env namespace."
  [sym {:keys [ns]}]
  (let [v (get-in (env/deref-env) [:namespaces ns :mappings (symbol (name sym))])]
    (if (and v (or (class? v)
                   (= ns (ns-name (.ns ^Var v) ))))
      v
      (let [meta (dissoc (meta sym) :inline :inline-arities #_:macro)
            meta (if-let [arglists (:arglists meta)]
                   (assoc meta :arglists (jvm/qualify-arglists arglists))
                   meta)]
        (intern ns (with-meta sym meta))))))

;; env is {:locals &env, :ns <some-ns>}
(defn macroexpand-all
  ([env form] (macroexpand-all [] env form))
  ([passes env form]
   (let [forms         (atom {})
         macroexpanded (if (cljs? env)
                         (let [ast (binding [a/*cljs-warnings* a/*cljs-warnings* #_(assoc a/*cljs-warnings* :undeclared-var false)]
                                     (with-redefs [;; a/elide-analyzer-meta (fn [m] (dissoc m ::a/analyzed ::form))
                                                   a/resolve-existing-var (fn resolve [env sym] (a/resolve-var env sym)) ;; disable warnings
                                                   a/macroexpand-1 (fn macroexpand-1 [env form]
                                                                     (let [form' (a/macroexpand-1* env form)]
                                                                       (when (not= form form')
                                                                         (swap! forms assoc form' form))
                                                                       form'))]
                                       (apply-passes passes env (aa/analyze env form))))]
                           (emit ast))
                         (with-redefs [jvm/macroexpand-1   (fn [form env]
                                                             (let [form' (macroexpand-1* form env)]
                                                               (when (not= form form')
                                                                 (swap! forms assoc form' form))
                                                               form'))
                                       emit-form/emit-form #(emit-form* % #{:qualified-symbols :hygienic})]
                           (binding [jvm/run-passes (comp emit-form/emit-form
                                                          (partial apply-passes passes env))]
                             (let [locals (into {} (map coerce-local (:locals env)))]
                               (jvm/analyze form (-> (jvm/empty-env)
                                                     (update :locals merge locals)
                                                     (update :ns #(or (ensure-ns (:ns env)) %))))))))]
     [@forms macroexpanded])))

(defmethod emit-form/-emit-form :def
  [ast opts]
  (let [f (default/-emit-form ast opts)]
    (if (:qualified-symbols opts)
      `(def ~(with-meta (symbol (-> ast :env :ns name) (str (second f)))
               (meta (second f)))
         ~@(nthrest f 2))
      f)))

(defn in-current-ns? [ast]
  (when (some? (:ns (:meta ast)))
    (= (symbol (str (:ns (:meta ast))))
       (:ns (:env ast)))))

(defmethod emit-form/-emit-form :var
  [{:keys [form ^clojure.lang.Var var] :as ast} opts]
  (default/-emit-form ast opts))

(defmethod default/-emit-form :var
  [{:keys [form var] :as ast} opts]
  (if-not (in-current-ns? ast)
    (with-meta (symbol (-> var .ns ns-name name) (-> var .sym name))
      (meta form))
    form))

(defmethod default/-emit-form :quote
  [{:keys [expr]} opts]
  (let [form (list 'quote (default/-emit-form* expr opts))]
    (with-meta form {::form form})))

(defmethod default/-emit-form :let
  [{:keys [bindings body]} opts]
  `(let* [~@(default/emit-bindings bindings opts)]
     (do ~(default/-emit-form* body opts))))
