
(declare config)
(def ^:dynamic *config*)
(def ^:dynamic *currently-loading*)

(def default-config
  "Any config you may provide to minitest will merge into this base
  configuration map.

  See `(source default-config)`."
  {:fail-early         false
   :silent-success     false
   :load-tests         true
   :dir                "test"
   :on-load {:store    true
             :run      false}
   :on-eval {:store    false
             :run      true}
   :runner   {:class   minitest.Runner}
   :reporter {:class   minitest.TermReporter
              :success {:logo '✅} ;; :dots false
              :failure {:logo '❌} ;; :dots false
              :error   {:logo '🔥} ;; :dots false
              :dots    false}
   :profiles {:production  {:load-tests       false}
              :development {:break-on-failure true}
              :cli         {:reporter {:dots  true}}
              :ci          [:cli]}})

(defn- read-config []
  (let [f (io/file "./minitest.edn")]
    (-> (or (when (.exists f) f)
            (io/resource "minitest.edn"))
        (some-> slurp edn/read-string))))

(def ^:dynamic *config* (read-config))

;; Taken from https://gist.github.com/danielpcox/c70a8aa2c36766200a95
(defn- deep-merge [& maps]
  (apply merge-with (fn [& args]
                      (if (every? #(or (map? %) (nil? %)) args)
                        (apply deep-merge args)
                        (last args)))
         maps))

(defn- profile-config [x]
  (condp call x
    map?        x
    sequential? (->> x
                     (map profile-config)
                     (apply deep-merge))
    (-> *config* :profiles (get x))))

(defn config
  ([] (config *profile*))
  ([profile]
   (-> (deep-merge default-config
                   (dissoc *config* :profiles)
                   (profile-config profile))
       (as-> m (deep-merge
                 m (dissoc (get m (if *currently-loading* :on-load :on-eval))
                           :store :run))))))
