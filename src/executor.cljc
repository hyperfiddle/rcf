(declare run-test-and-yield-report!)

(defn execute-clj [state position level ns-name data]
  (case [position level]
    [:before :suite]  nil
    [:before :ns]     nil
    [:before :block]  nil
    [:before :case]   nil
    [:do     :case]   (run-test-and-yield-report! ns-name data)
    [:after  :case]   nil
    [:after  :block]  nil
    [:after  :ns]     nil
    [:after  :suite]  nil))


(def testing-repl nil)

(macros/deftime
  (defn- start-testing-repl! []
    (assert (nil? testing-repl) "Already started")
    (alter-var-root #'testing-repl
                    (constantly
                      (macros/case
                        :clj  (do (println "STARTING TESTING REPL") (cljs-prepl))
                        :cljs nil)))
    (with-repl testing-repl (~'require '~'minitest))))

;; TODO: handle src-dirs
; (defn ns-paths [platform]
;   (->> (cp/classpath)
;        (filter (memfn ^java.io.File isDirectory))
;        (mapcat (fn [dir]
;                  (for [f (find-sources-in-dir dir)
;                        :let [decl (read-file-ns-decl f (:read-opts platform))]
;                        :when decl]
;                    [(name-from-ns-decl decl)
;                     (.getAbsolutePath f)])))
;        (into {})))

(defn execute-cljs [state position level ns-name data]
  (case [position level]
    [:before :suite]
    (macros/case
      :clj (when-not testing-repl
             (start-testing-repl!)
             (with-repl testing-repl
               (~'require '~'minitest :reload) ;; TODO: do not reload minitest
               (~'use '~'cljs.core) ;; TODO: required ? Else move elsewhere
               (~'use '~'[cljs.repl :only [doc source error->str]]))))

    [:before :ns]
    (macros/case
      :clj  (let [res (with-repl testing-repl
                        ;; TODO: reload only if reloading in clj
                        (~'require '~ns-name :reload))]
              (dbg "REPL RESULT" res)))

    [:before :block]  nil
    [:before :case]   nil

    [:do  :case]
    (macros/case
      :cljs (run-test-and-yield-report! ns-name data)
      :clj  (with-repl testing-repl
              (run-test-and-yield-report!
                '~ns-name
                ~(let [a assoc-in  u update-in  d data]
                   (-> d
                       (u [:tested   :form]    #(do `(quote ~%)))
                       (u [:expected :form]    #(do `(quote ~%)))
                       (a [:tested   :thunk]   (-> d :tested   :form as-thunk))
                       (a [:expected :thunk]   (-> d :expected :form as-thunk)))
                   ))))

    [:after  :case]   nil
    [:after  :block]  nil
    [:after  :ns]     nil
    [:after  :suite]  nil))
