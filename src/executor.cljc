(declare run-test-and-yield-report!)

(def testing-repl (atom nil))

(def execute-clj
  (on| [:do :case] (fn [state position level ns data]
                     (run-test-and-yield-report! ns data))))

(macros/deftime
  (defn- start-testing-repl! []
    (assert (nil? @testing-repl) "Already started")
    (macros/case
      :clj  (let [repl (cljs-prepl)]
              (println "STARTING TESTING REPL")
                (reset! testing-repl repl)
                (with-repl repl (~'require '~'minitest)))
      :cljs (println "ATTEMPTED TO START THE REPL IN CLJS"))))

(defn ensure-testing-repl [state position level ns-name data]
  (macros/case
    :clj (when-not @testing-repl
           (start-testing-repl!)
           (println "testing-repl" @testing-repl)
           (with-repl @testing-repl
             (~'require '~'minitest :reload) ;; TODO: do not reload minitest
             (~'use '~'cljs.core) ;; TODO: required ? Else move elsewhere
             (~'use '~'[cljs.repl :only [doc source error->str]])))))

(defn require-tested-ns-in-cljs [state position level ns data]
  (do (println "TESTING REPL" @testing-repl)
      (println "ENV" (macros/case  :clj :clj  :cljs :cljs))
      #_(macros/case
          :clj  (let [res (with-repl @testing-repl
                            ;; TODO: reload only if reloading in clj
                            (~'require '~ns :reload))]
                  (dbg "REPL RESULT" res)))))

(defn run-case-in-cljs [state position level ns data]
  (macros/case
    :cljs (run-test-and-yield-report! ns-name data)
    :clj  (with-repl @testing-repl
            (run-test-and-yield-report!
              '~ns-name
              ~(let [a assoc-in  u update-in  d data]
                 (-> d
                     (u [:tested   :form]    #(do `(quote ~%)))
                     (u [:expected :form]    #(do `(quote ~%)))
                     (a [:tested   :thunk]   (-> d :tested   :form as-thunk))
                     (a [:expected :thunk]   (-> d :expected :form as-thunk)))
                 )))))

(def execute-cljs
  (outside-in->> (on| [:before :suite] ensure-testing-repl)
                 (on| [:before :ns]    require-tested-ns-in-cljs)
                 (on| [:do     :case]  run-case-in-cljs)))
