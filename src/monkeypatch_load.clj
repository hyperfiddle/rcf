(require '[robert.hooke :refer [add-hook]])

(defn clear-tests!        [a & nss]    (swap! a #(apply dissoc % nss)))
(defn add-tests!          [a ns blocs] (swap! a update ns concat blocs))
(defn store-tests!        [ns blocs]   (add-tests! *tests*            ns blocs))
(defn process-after-load! [ns blocs]   (add-tests! *tests-to-process* ns blocs))

(defn process-tests-on-load-now! []
  (assert *currently-loading*)
  (try
    (let [conf (config)]
      (when (:store conf) (run! #(apply store-tests! %) @*tests-to-process*))
      (when (:run conf)   (run-and-report! :suite @*tests-to-process*)))
    (finally
      (reset! *tests-to-process* nil))))

(defn- around-load-hook [orig-load & paths]
  (with-contexts {:exec-mode :load}
    (binding [*currently-loading* true
              *tests-to-process* (atom nil)]
      (let [conf        (config)
            nss         (map #(-> (str/replace #"^/" "" %)
                                  (str/replace "/" ".")
                                  symbol)
                             paths)
            load-result (do (apply clear-tests! *tests* nss)
                            (ensuring-runner&reporter (apply orig-load paths)))]
        (when (or (:store conf) (:run conf))
          (process-tests-on-load-now!))
        load-result))))

(defn- apply-patch-to-load []
  (add-hook #'clojure.core/load #'around-load-hook))
