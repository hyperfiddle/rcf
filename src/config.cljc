;; Yo, the default config map is in minitest.cljc around line 200
(declare default-config)

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

;; TODO: private
(def ^:dynamic *config*        (file-config))
(def ^:dynamic *forced-config* nil)
(def ^:dynamic *context*       nil)

(defn config!
  ([k v & {:as more}] (config! (assoc more k v)))
  ([m]                (macros/case
                        :clj  (alter-var-root #'*config* merge m)
                        :cljs (throw (e-info "Not implemented for cljs")))))
(defn force-config!
  ([k v & {:as more}] (force-config! (assoc more k v)))
  ([m]                (macros/case
                        :clj  (alter-var-root #'*forced-config* merge m)
                        :cljs (throw (e-info "Not implemented for cljs")))))
(defn context!
  ([k v & {:as more}] (context! (assoc more k v)))
  ([m]                (macros/case
                        :clj  (alter-var-root #'*context* merge m)
                        :cljs (throw (e-info "Not implemented for cljs")))))

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
    (do         (some-> m :CONTEXT (get x) (->> (parse-profile m))))))

(defn context-map? [x]
  (and (map? x) (contains? x :CONTEXT)))

(defn contextualize [m ctx]
  (->> m (clojure.walk/postwalk
           (fn [form]
             (if-not (context-map? form)
               form
               (let [c           (:CONTEXT form)
                     active-ctx  (merge (:DEFAULT-CONTEXT form)
                                        (select-keys ctx (keys c)))
                     c-ms        (map #(parse-profile form (get-in c %))
                                      active-ctx)
                     new-m       (apply deep-merge form c-ms)]
                 ;; Since a context can bring in new contexts, contextualize
                 ;, again until fix point is reached
                 (if (= form new-m)
                   new-m
                   (contextualize new-m ctx))))))))

(defn config [& [conf]]
  (-> (deep-merge default-config *config* conf)
      (contextualize *context*)
      (deep-merge *forced-config*)))

(macros/deftime
  (defmacro with-config [m & body]
    `(binding [*config* (deep-merge *config* ~m)]
       ~@body))

  (defmacro with-context [m & body]
    `(binding [*context* (merge *context* ~m)]
       ~@body)))

