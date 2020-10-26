
(declare default-config)

(defn- config-file []
  (let [f (io/file "./minitest.edn")]
    (or (when  (and (.exists f) (-> f slurp edn/read-string empty? not))  f)
        (io/resource "minitest.edn"))))

(defn- file-config []
  (some-> (config-file) slurp edn/read-string))

(def ^:dynamic *config* (file-config))

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
    (do         (some-> m :contexts (get x) (->> (parse-profile m))))))

(defn context-map? [x]
  (and (map? x) (contains? x :contexts)))

(defn contextualize [ctxs m]
  (->> m (clojure.walk/postwalk
           (fn [form]
             (if-not (context-map? form)
               form
               (let [c           (:contexts form)
                     active-ctxs (select-keys ctxs (keys c))
                     c-ms        (map #(parse-profile form (get-in c %))
                                      active-ctxs)]
                 (-> (apply deep-merge form c-ms)
                     ;; prevent merging contexts from contexts
                     (assoc :contexts (:contexts form)))))))))

(defn config
  [& [conf]]
  (->> [default-config *config* conf]
       (apply deep-merge)
       (contextualize *contexts*)))

(defmacro with-config [m & body]
  `(binding [*config* (deep-merge *config* ~m)]
     ~@body))

(defmacro with-contexts [m & body]
  `(binding [*contexts* (merge *contexts* ~m)]
     ~@body))

