(ns minitest.config
  (:require
    #?(:clj [clojure.java.io        :as    io])
    #?(:clj [clojure.edn            :as    edn])
    #?(:clj [clojure.tools.reader   :as    r])
            [clojure.walk           :refer [prewalk postwalk]]
            [net.cgrand.macrovich   :as    macros]
            [minitest.base-config   :refer [base-config]]
            [minitest.utils         :refer [call func-map filtered-map]])
  #?(:cljs (:require-macros
             [minitest.config       :refer [file-config defaccessors memo]])))

;; TODO: assert config keys.
;; TODO: config should be lazy

(macros/deftime
  ;; TODO: clarify where the config file should be
  (defn config-file []
    (let [f (io/file "./minitest.edn")]
      (or (when  (and (.exists f) (-> f slurp edn/read-string empty? not))
            f)
          (io/resource "minitest.edn"))))

  ;; TODO: it does not eval/resolve symbols -> hence can't place function and
  ;; other vars in file configs
  (defmacro ^:private file-config []
    ;; TODO: remove case and use cljs version for both
    (macros/case
      :clj  `(some-> (config-file) slurp edn/read-string)
      :cljs `(quote ~(some-> (config-file) slurp edn/read-string)))))

(defonce config-memo (atom {}))

(macros/deftime
  (defmacro memo [ks vals & body]
    `(let [full-path# (concat ~ks ~vals)
           already#   (get-in @config-memo full-path# ::not-found)]
       (if (= already# ::not-found)
         (doto (do ~@body)
               (->> (swap! config-memo assoc-in full-path#)))
         already#))))

;; Taken from https://gist.github.com/danielpcox/c70a8aa2c36766200a95
;; TODO: acknowledge.
(defn ^:no-doc deep-merge [& maps]
  (memo [:deep-merge] maps
        (reduce (partial merge-with (fn [x y]
                                      (if (every? #(or (map? %) (nil? %)) [x y])
                                        (deep-merge x y)
                                        y)))
                maps)))

(def ^:dynamic *early-config*  (file-config))
(def ^:dynamic *late-config*   nil)
(def ^:dynamic *context*       nil)


;; Taken from https://stackoverflow.com/questions/14488150/how-to-write-a-dissoc-in-command-for-clojure
(defn- dissoc-in [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(macros/deftime
  (defmacro ^:private defaccessors
    [name var-name & {:keys [getter setter clearer binder unbinder]
                      :or   {getter   true setter true clearer true binder true
                             unbinder true}}]
    (let [getter-name   name
          setter-name   (symbol (str             name "!"))
          clearer-name  (symbol (str "clear-"    name "!"))
          binder-name   (symbol (str "with-"     name))
          unbinder-name (symbol (str "without-"  name))]
      `(do ~(when getter  `(defn ~getter-name [] ~var-name))
           ~(when setter  `(defn ~setter-name
                             ([k# v# & {:as more#}]
                              (~setter-name (assoc more# k# v#)))
                             ([m#]
                              (macros/case
                                :clj  (alter-var-root #'~var-name merge m#)
                                :cljs (set! ~var-name (merge ~var-name m#)))
                              nil)))
           ~(when clearer `(defn ~clearer-name
                             ([]
                              (macros/case
                                :clj  (alter-var-root #'~var-name
                                                      (constantly nil))
                                :cljs (set! ~var-name nil))
                              nil)))
           ~(when binder  `(macros/deftime
                             (defmacro ~binder-name [~'m# & ~'body#]
                               (r/syntax-quote
                                 (binding
                                   [~var-name (deep-merge
                                                ~var-name
                                                (~'clojure.core/unquote ~'m#))]
                                   (~'clojure.core/unquote-splicing
                                     ~'body#))))))
           ~(when unbinder `(macros/deftime
                             (defmacro ~unbinder-name [~'ks# & ~'body#]
                               (r/syntax-quote
                                 (binding
                                   [~var-name (dissoc-in
                                                ~var-name
                                                (~'clojure.core/unquote ~'ks#))]
                                   (~'clojure.core/unquote-splicing
                                     ~'body#))))))))))

(defn- parse-WHEN-val [m x]
  (condp call x
    map?        x
    sequential? (->> (map (partial parse-WHEN-val m) x)
                     (reduce deep-merge))
    (do         (some-> m :WHEN (get x) (->> (parse-WHEN-val m))))))

(defn- when-map? [x]
  (and (map? x) (contains? x :WHEN)))

(defn contextualize [m ctx]
  (memo
    [:contextualize] [m ctx]
    (->> m
         (postwalk
           (fn [form]
             (if-not (when-map? form)
               form
               (let [when-map   (:WHEN form)
                     active-ctx (deep-merge (:CTX form)
                                            (select-keys ctx (keys when-map)))
                     ms         (map #(parse-WHEN-val form (get-in when-map %))
                                     active-ctx)
                     new-form   (reduce deep-merge (cons form ms))]
                 ;; Since a when-map can bring in another one, contextualize
                 ;, again until fix point is reached
                 (if (= form new-form)
                   new-form
                   (contextualize new-form ctx)))))))))

(defaccessors default-config *early-config*)
(defaccessors config         *late-config*  :getter false)
(defaccessors context        *context*      :getter false)

(defn- ns-config [] (some-> *context* :ns find-ns meta :minitest/config))

(declare config)
(defn context [& [ctx default-ctx]]
  (reduce deep-merge [(or default-ctx (:CTX (config :finalize false)))
                      *context*
                      ctx]))

(defn  config [& {:keys [finalize] :or {finalize true}}]
  (let [srcs     [base-config *early-config* (ns-config) *late-config*]
        merged   (reduce deep-merge srcs)
        base-ctx (as-> (:CTX merged {}) $
                   (contextualize $ $))
        ctx      (context nil (deep-merge merged base-ctx))
        result   (->> (concat srcs [ctx])
                      (map #(contextualize % ctx))
                      (reduce deep-merge))]
    (if finalize
      (-> result
          (filtered-map [:CTX :WHEN])
          (func-map))
      result)))

