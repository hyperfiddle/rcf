;; TODO: assert config keys.
;; TODO: config should be lazy

;; Yo, the default config map is in minitest.cljc around line 200
(declare base-config)

(macros/deftime
  (defn- config-file []
    (let [f (io/file "./minitest.edn")]
      (or (when  (and (.exists f) (-> f slurp edn/read-string empty? not))
            f)
          (io/resource "minitest.edn"))))

  (defmacro ^:private file-config []
    ;; TODO: remove case and use cljs version for both
    (macros/case
      :clj  `(some-> (config-file) slurp edn/read-string)
      :cljs `(quote ~(some-> (config-file) slurp edn/read-string)))))

;; Taken from https://gist.github.com/danielpcox/c70a8aa2c36766200a95
;; TODO: acknowledge.
(defn ^:no-doc deep-merge [& maps]
  (apply merge-with (fn [& args]
                      (if (every? #(or (map? %) (nil? %)) args)
                        (apply deep-merge args)
                        (last args)))
         maps))

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

(defn at-runtime? [coll] (-> coll flatten first (= :AT-RUNTIME)))

(defn- parse-WHEN-val [m x]
  (condp call x
    map?        x
    sequential? (if (at-runtime? x)
                  (->> (map (partial parse-WHEN-val m) x)
                       (apply deep-merge)))
    (do         (some-> m :WHEN (get x) (->> (parse-WHEN-val m))))))

(defn- ctx-map? [x]
  (and (map? x) (contains? x :WHEN)))

(defn contextualize [m ctx & [cfg]]
  (->> m (clojure.walk/postwalk
           (fn [form]
             (if-not (ctx-map? form)
               form
               (let [when-map   (:WHEN form)
                     active-ctx (deep-merge (:CTX form)
                                            (select-keys ctx (keys when-map)))
                     ms         (map #(parse-WHEN-val form (get-in when-map %))
                                     active-ctx)
                     new-form   (apply deep-merge form ms)]
                 ;; Since a when-map can bring in another one, contextualize
                 ;, again until fix point is reached
                 (if (= form new-form)
                   new-form
                   (contextualize new-form ctx cfg))))))))

(defaccessors default-config *early-config*)
(defaccessors config         *late-config*  :getter false)
(defaccessors context        *context*      :getter false)

(defn- ns-config [] (some-> *context* :ns find-ns meta :minitest/config))

(defn context    [& [ctx default-ctx]]
  (deep-merge (or default-ctx (:CTX (config)))
              *context*
              ctx))

(defn  config    [& [conf]]
  (let [srcs        [base-config *early-config* (ns-config) conf *late-config*]
        merged      (apply deep-merge srcs)
        base-ctx    (as-> (:CTX merged {}) $
                      (contextualize $ $ merged))
        ctx         (context nil base-ctx)]
    (->> (concat srcs [ctx])
         (map #(contextualize % ctx))
         (apply deep-merge))))

