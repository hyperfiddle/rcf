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

;; TODO: private
(def ^:dynamic *early-config*  (file-config))
(def ^:dynamic *late-config*   nil)
(def ^:dynamic *context*       nil)

(macros/deftime
  (defmacro ^:private def-config-variable [name var-sym]
    (let [getter-sym  name
          setter-sym  (symbol (str name "!"))
          clearer-sym (symbol (str "clear-" name "!"))]
      `(do (defn ~getter-sym  []    ~var-sym)
           (defn ~setter-sym
             ([k# v# & {:as more#}] (~setter-sym (assoc more# k# v#)))
             ([m#]                  (macros/case
                                      :clj  (alter-var-root #'~var-sym merge m#)
                                      :cljs (throw (ex-info
                                                     "Not implemented for cljs" {}
                                                     )))
              nil))
           (defn ~clearer-sym []    (macros/case
                                      :clj  (alter-var-root #'~var-sym
                                                            (constantly nil))
                                      :cljs (throw (ex-info
                                                     "Not implemented for cljs" {}
                                                     )))
             nil)))))

; (def-config-variable default-config *early-config*)
; (def-config-variable config         *late-config*) ;; the getter is redef below
; (def-config-variable context        *context*) ;; ditto

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
  ; (->> [base-config *early-config* conf *late-config*]
  ;      (reduce (fn [m1 m2]
  ;                (deep-merge m1))))
  (-> (deep-merge base-config *early-config* conf *late-config*)
      (contextualize *context*)))

(macros/deftime
  (defmacro with-config [m & body]
    `(binding [*early-config* (deep-merge *early-config* ~m)]
       ~@body))

  (defmacro with-context [m & body]
    `(binding [*context* (merge *context* ~m)]
       ~@body)))

