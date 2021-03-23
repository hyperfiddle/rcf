(ns minitest.utils
  (:require [net.cgrand.macrovich         :as    macros]
   #?(:clj  [cljs.analyzer                :as    ana])
            [clojure.walk                 :refer [postwalk]]
            [camel-snake-kebab.core       :refer [->kebab-case]]
   #?(:cljs [minitest.with-bindings       :refer [with-bindings*]]))
  #?(:cljs
      (:require-macros [minitest.utils   :refer [env-case]])))

(def ->|               #(apply comp (reverse %&)))
(def not|              complement)
(def call              #(apply %1 %&))
(def as-form           #(do `'~%))
(def as-thunk          #(do `(fn [] ~%)))
(def as-wildcard-thunk #(do `(fn [] (let [~'_ '~'_] ~%))))

(defn current-bindings []
  (->> (macros/case
         :clj  (get-thread-bindings)
         :cljs (get-all-dyn-bindings))
       (into {})))

(macros/deftime
  (defmacro current-file []
    (if (-> *file* first (= \/))
      *file*
      (-> *file* ClassLoader/getSystemResource .getPath)))

  (defmacro current-ns-name []
    `'~(ns-name *ns*))

  ;; Taken from:
  ;; https://github.com/ptaoussanis/encore/blob/009da50b8d38ebd00db0ec99bd99312cf0c56203/src/taoensso/encore.cljc#L374
  ;; TODO: keep ?
  (defmacro defalias
    "Defines an alias for a var, preserving its metadata."
    ([    src      ] `(defalias ~(symbol (name src)) ~src nil))
    ([sym src      ] `(defalias ~sym                 ~src nil))
    ([sym src attrs]
     (let [attrs (if (string? attrs) {:doc attrs} attrs)] ; Back compatibility
       `(let [attrs# (conj (select-keys (meta (var ~src))
                                        [:doc :arglists :private :macro])
                           ~attrs)]
          (alter-meta! (def ~sym @(var ~src)) conj attrs#)
          (var ~sym))))))

;; Taken from https://stackoverflow.com/questions/14488150/how-to-write-a-dissoc-in-command-for-clojure
(defn dissoc-in [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

;; TODO: for cljs, see
;; https://developer.mozilla.org/en-US/docs/Web/API/console#usage,
;; section 'Styling console output'
(defn emphasize [s]
  (macros/case
    :clj  (str "\033[1m" s "\033[0m")
    :cljs (str/upper-case s)))

(macros/deftime
  (defmacro env-case [env & {:keys [cljs clj]}]
    `(let [env# ~env]
       (if (contains? env# '~'&env)
          (if (:ns env#) ~cljs ~clj)
          (if #?(:clj (:ns env#) :cljs true)
            ~cljs
            ~clj))))

  (defn meta-macroexpand-1 [env form]
    (env-case env
              :clj  ((ns-resolve 'clojure.core 'macroexpand-1 )form)
              :cljs (if (sequential? form)
                      (ana/macroexpand-1* env form)
                      form)))

  (defn meta-macroexpand [env form]
    (let [ex   (if-let [m (meta form)]
                 (vary-meta (meta-macroexpand-1 env form)
                            #(merge m %))
                 (meta-macroexpand-1 env form))]
      (if (identical? ex form)
        form
        (meta-macroexpand env ex)))))

;; TODO: keep ?
(def ^:dynamic *top-levels* {})

(macros/deftime
  (defmacro tracking-top-level
    ([expr]    `(tracking-top-level ~(gensym "tracking-top-level-id-") ~expr))
    ([id expr] `(let [~'&top-level-id ~id]
                  (binding [*top-levels* (update *top-levels* ~'&top-level-id
                                                 (fnil inc -1))]
                    ~expr))))

  (defmacro top-level?
    ([]   `(top-level? ~'&top-level-id))
    ([id] `(zero? (*top-levels* ~id)))))
