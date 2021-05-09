(ns minitest.utils
  (:require [net.cgrand.macrovich         :as    macros]
   #?(:clj  [cljs.analyzer                :as    ana])
   #?(:cljs [clojure.string               :as    str])
            [clojure.walk                 :refer [postwalk]]
            [camel-snake-kebab.core       :refer [->kebab-case]]
   #?(:cljs [minitest.with-bindings       :refer [with-bindings*
                                                  get-all-dyn-bindings]]))
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

(def reduce1
  (macros/case
    :clj  @#'clojure.core/reduce1
    :cljs reduce))

(defn gen-uuid []
  (macros/case
    :clj  (str (java.util.UUID/randomUUID))
    :cljs (random-uuid)))

(defn merge-entries-with
  "Like `merge-with` except that `f` is passed entries instead of values."
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e)]
                          (if (contains? m k)
                            (assoc m k (f (find m k) e))
                            (assoc m k (val e)))))
          merge2 (fn [m1 m2]
                   (reduce1 merge-entry (or m1 {}) (seq m2)))]
      (reduce1 merge2 maps))))

(defn minitest-var? [var]
  (when-let [s (some->> var meta :ns ns-name str)]
    (and      (re-matches #"minitest.*" s)
         (not (re-matches #"minitest.*-test" s)))))

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
        (meta-macroexpand env ex))))

  (defmacro with-out-str+result [& body]
    (macros/case
      :clj  `(let [s# (java.io.StringWriter.)]
               (binding [*out* s#]
                 (let [result# (do ~@body)]
                   [(str s#) result#])))
      :cljs `(let [s# (goog.string/StringBuffer.)]
               (binding [cljs.core/*print-newline* true
                         cljs.core/*print-fn*      (fn [x#] (.append s# x#))]
                 (let [result# ~@body]
                   [(str s#) result#]))))))

;; Taken from from https://gist.github.com/danielpcox/c70a8aa2c36766200a95
(defn dmerge "Simple deep-merge" [& maps]
  (apply merge-with (fn [& args]
                      (if (every? #(or (map? %) (nil? %)) args)
                        (apply dmerge args)
                        (last args)))
         maps))

(macros/deftime
  (defmacro set-var! [var-name val]
    (macros/case
      :clj  (do (assert (.isDynamic (resolve var-name)))
                `(if (thread-bound? #'~var-name)
                   (set! ~var-name ~val)
                   (alter-var-root #'~var-name (constantly ~val))))
      :cljs `(set! ~var-name ~val))))
