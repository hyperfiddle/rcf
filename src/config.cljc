;; TODO: assert config keys.

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

(defn- parse-profile [m x]
  (condp call x
    map?        x
    sequential? (->> (map (partial parse-profile m) x)
                     (apply deep-merge))
    (do         (some-> m :WHEN (get x) (->> (parse-profile m))))))

(defn context-map? [x]
  (and (map? x) (contains? x :WHEN)))

(defn contextualize [m ctx]
  (->> m (clojure.walk/postwalk
           (fn [form]
             (if-not (context-map? form)
               form
               (let [c           (:WHEN form)
                     active-ctx  (merge (:DEFAULT-CTX form)
                                        (select-keys ctx (keys c)))
                     c-ms        (map #(parse-profile form (get-in c %))
                                      active-ctx)
                     new-m       (apply deep-merge form c-ms)]
                 ;; Since a context can bring in new contexts, contextualize
                 ;, again until fix point is reached
                 (if (= form new-m)
                   new-m
                   (contextualize new-m ctx))))))))

(def ^:dynamic *early-config*  (file-config))
(def ^:dynamic *late-config*   nil)
(def ^:dynamic *context*       nil)

(macros/deftime
  (defmacro ^:private defaccessors
    [name var-name & {:keys [getter setter clearer binder binder-f]
                      :or   {getter   true setter true clearer true binder true
                             binder-f '(fn [old new] new)}}]
    (let [getter-name  name
          setter-name  (symbol (str          name "!"))
          clearer-name (symbol (str "clear-" name "!"))
          binder-name  (symbol (str "with-"  name))]
      `(do ~(when getter  `(defn ~getter-name [] ~var-name))
           ~(when setter  `(defn ~setter-name
                             ([k# v# & {:as more#}]
                              (~setter-name (assoc more# k# v#)))
                             ([m#]
                              (macros/case
                                :clj  (alter-var-root #'~var-name merge m#)
                                :cljs (set! ~var-name (merge ~var-name m#)))
                              nil)))
           ~(when clearer `(defn ~clearer-name []
                             (macros/case
                               :clj  (alter-var-root #'~var-name
                                                     (constantly nil))
                               :cljs (set! ~var-name nil))
                             nil))
           ~(when binder  `(macros/deftime
                             (defmacro ~binder-name [~'m# & ~'body#]
                               (r/syntax-quote
                                 (binding
                                   [~var-name (~binder-f
                                                ~var-name
                                                (~'clojure.core/unquote ~'m#))]
                                   (~'clojure.core/unquote-splicing
                                     ~'body#))))))))))

(defaccessors default-config *early-config*)
(defaccessors config         *late-config*  :getter false  :binder-f deep-merge)
(defaccessors context        *context*      :getter false  :binder-f deep-merge)
(defn         config         [& [conf]]     (-> (deep-merge
                                                  base-config *early-config*
                                                  conf        *late-config*)
                                                (contextualize *context*)))
(defn         context        [& [ctx]]      *context*
  #_(-> (deep-merge
                                                  (:DEFAULT-CTX (config))
                                                  *context*)
                                                (as-> ctx
                                                  (contextualize ctx ctx))))

