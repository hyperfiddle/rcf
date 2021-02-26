(ns minitest.executor
  (:require [net.cgrand.macrovich             :as           macros]
    #?(:clj [minitest.prepl                   :as           prepl])
    #?(:clj [minitest.utils                   :refer        [as-thunk]])
            [minitest.higher-order #?@(:clj  [:refer        [on-level|
                                                             outside-in->>]]
                                       :cljs [:refer        [on-level|]
                                              :refer-macros [outside-in->>]])]
            [minitest.dbg          #?@(:clj  [:refer        [dbg]]
                                       :cljs [:refer-macros [dbg]])]
            [minitest.runner                  :refer        [run-test-and-yield-report!]]))

(def testing-repl (atom nil))

(def execute-clj
  (on-level| [:do :case] (fn [state position level ns data]
                           (run-test-and-yield-report! ns data))))

(macros/deftime
  (defn- start-testing-repl! []
    (assert (nil? @testing-repl) "Already started")
    (macros/case
      :clj  (let [repl (prepl/cljs-prepl)]
              (println "STARTING TESTING REPL")
                (reset! testing-repl repl)
                (prepl/with-repl repl (~'require '~'minitest)))
      :cljs (println "ATTEMPTED TO START THE REPL IN CLJS"))))

(defn ensure-testing-repl [state position level ns-name data]
  (macros/case
    :clj (when-not @testing-repl
           (start-testing-repl!)
           (println "testing-repl" @testing-repl)
           (prepl/with-repl @testing-repl
             (~'require '~'minitest :reload) ;; TODO: do not reload minitest
             (~'use '~'cljs.core) ;; TODO: required ? Else move elsewhere
             (~'use '~'[cljs.repl :only [doc source error->str]]))))
  data)

(defn require-tested-ns-in-cljs [state position level ns data]
  (do (println "TESTING REPL" @testing-repl)
      (println "ENV" (macros/case  :clj :clj  :cljs :cljs))
      #_(macros/case
          :clj  (let [res (prepl/with-repl @testing-repl
                            ;; TODO: reload only if reloading in clj
                            (~'require '~ns :reload))]
                  (dbg "REPL RESULT" res))))
  data)

(defn run-case-in-cljs [state position level ns data]
  (macros/case
    :cljs (run-test-and-yield-report! ns-name data)
    :clj  (prepl/with-repl @testing-repl
            (run-test-and-yield-report!
              '~ns-name
              ~(let [a assoc-in  u update-in  d data]
                 (-> d
                     (u [:tested   :form]    #(do `(quote ~%)))  ;; TODO: use as-form ?
                     (u [:expected :form]    #(do `(quote ~%)))
                     (a [:tested   :thunk]   (-> d :tested   :form as-thunk))
                     (a [:expected :thunk]   (-> d :expected :form as-thunk)))
                 )))))

(def execute-cljs
  (outside-in->> (on-level| [:before :suite] ensure-testing-repl)
                 (on-level| [:before :ns]    require-tested-ns-in-cljs)
                 (on-level| [:do     :case]  run-case-in-cljs)))
