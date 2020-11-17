
(defn clear-tests!        [a & nss]    (swap! a #(apply dissoc % nss)))
(defn add-tests!          [a ns blocs] (swap! a update ns concat blocs))
(defn store-tests!        [ns blocs]   (add-tests! *tests*            ns blocs))
(defn process-after-load! [ns blocs]   (add-tests! *tests-to-process* ns blocs))

(defn process-tests-on-load-now! []
  (assert *currently-loading*)
  (try
    (let [conf (config)]
      (when (:store conf) (run! #(apply store-tests! %) @*tests-to-process*))
      (when (:run conf)   (run-execute-report! :suite @*tests-to-process*)))
    (finally
      (reset! *tests-to-process* nil))))

;; Clojure
(macros/case
  :clj (defn- around-clj-load-hook [orig-load & paths]
         (macros/case
           :clj  (println "LOADING"     paths)
           :cljs (println "NOT LOADING" paths))
         #?(:clj  (println "  & IN CLJ")
            :cljs (println "  & IN CLJS"))
         (with-contexts {:exec-mode :load}
           (binding [*currently-loading* true
                     *tests-to-process* (atom nil)]
             (let [conf        (config)
                   path->ns    (->> (all-ns)
                                    (mapv (juxt
                                            (->| str @#'clojure.core/root-resource)
                                            identity))
                                    (into {}))
                   nss         (map (->|  path->ns str symbol) paths)
                   load-result (do (apply clear-tests! *tests* nss)
                                   (ensuring-runner+executors+reporter
                                     (apply orig-load paths)))]
               (when (or (:store conf) (:run conf))
                 (process-tests-on-load-now!))
               load-result)))))

(macros/case
  :clj (defn- apply-patch-to-clojure-core-load []
         (add-hook #'clojure.core/load
                   #'around-clj-load-hook)))

;; ClojureScript
; #?(:clj
;     (defn around-cljs-emit-hook [orig-emit & args]
;       (apply orig-emit args)))

; #?(:clj (defn- apply-patch-to-cljs-compiler-emit []
;           (add-hook #'cljs.compiler/emit
;                     #'around-cljs-emit-hook)))
