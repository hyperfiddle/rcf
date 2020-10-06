(require '[robert.hooke :refer [add-hook]])

(def ^:no-doc ^:dynamic *currently-loading* false)
(def ^:no-doc ^:dynamic *tests-to-process*  nil)

(defn- clear-tests [ns-name]
  (swap! *tests* dissoc ns-name))

(declare process-tests-on-load-now!)
(defn- hook-around-load [orig-load & paths]
  (binding [*currently-loading* true
            *tests-to-process* (atom nil)]
    (let [nss    (->> paths (map #(-> %
                                      (str/replace #"^/" "")
                                      (str/replace "/" ".")
                                      symbol)))
          result (do (run! clear-tests nss)
                     (apply orig-load paths))
          conf   (config)]
      (when (or (-> conf :on-load :store)
                (-> conf :on-load :run))
        (run! #(apply process-tests-on-load-now! %) @*tests-to-process*))
      result)))

(when (load-tests?)
  (add-hook #'clojure.core/load #'hook-around-load))
