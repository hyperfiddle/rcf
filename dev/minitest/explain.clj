(ns minitest.explain
  (:gen-class)
  (:require [dorothy.core :as dodo
                          :refer [dot graph-attrs edge-attrs node-attrs]]
            [dorothy.jvm  :refer [render save! show!]]))


(def A :aquamarine)
(def B :lightpink)
(def C :orange)

(defn splice          [x] [::splice x])
(defn spliced-content [x] (second x))
(defn splice?         [x] (and (vector? x) (-> x first (= ::splice))))

(defn compose-graph [items]
  (reduce (fn [acc i]
            (if (splice? i)
              (into acc (spliced-content i))
              (conj acc i)))
          [] items))

(defn splits [pred coll]
  [(split-with pred coll)])

(defn attrs [ms & edges]
  (splice (for [e     edges
                m ms
                :let [[src dst attrs] e]]
            [src dst (merge attrs m)])))

(defn colors [cs & edges]
  (apply  attrs  (map #(do {:color %}) cs)  edges))

(defn digraph [items]
  (dodo/digraph (compose-graph items)))

(defn subgraph [name items]
  (dodo/subgraph name (compose-graph items)))

(def code-paths
  (digraph
    [(graph-attrs {:pack "true"})
     (node-attrs  {:shape :box})

     (subgraph
       :cluster_via_load
       [{:label "Code whose evaluation is handled by clojure.core/load"}

        [:clj-repl-require         {:label "user=> (require 'my-namespace)"        :color A}]
        [:java-jar                 {:label "$ java -jar my-app.jar"                :color A}]])

     (subgraph
       :cluster_via_eval
       [{:label "Code directly evaluated with eval"}

        [:tests-from-clj-repl {:label "user=> (tests ...)"                         :color B}]
        [:tests-from-cider    {:label "M-x cider-eval-sexp-at-point on (tests...)" :color B}]])

     [:currently-loading!       {:label "(binding [*currently-loading* true] ..."}]
     [:currently-loading?       {:label "*currently-loading* ?"}]
     [:setup-*tests-to-process* {:label "(binding [*tests-to-process* (atom nil)] ..."}]
     [:clear-*tests*            {:label "(apply clear-tests! *tests* nss)"}]
     [:ensuring-runner          {:label "(ensuring-runner+executors+reporter ..."}]

     (colors [A]
             [:clj-repl-require    :currently-loading!]
             [:java-jar            :currently-loading!])

     (subgraph :cluster_around-clj-load-hook-before
               [{:label "around-clj-load-hook (before)"}

                (colors [A]
                        [:currently-loading!       :setup-*tests-to-process*]
                        [:setup-*tests-to-process* :clear-*tests*]
                        [:clear-*tests*            :ensuring-runner]
                        [:ensuring-runner          :load])])

     (colors [A] [:load :eval])

     (colors [B]
             [:tests-from-clj-repl :eval]
             [:tests-from-cider    :eval])

     (colors [A B]
             [:eval                           "macroexpanding (tests...)"]
             ["macroexpanding (tests...)"     "(load-tests?)"]

             ["(load-tests?)"       "macroexpands to ..."  {:label "true"}]
             ["(load-tests?)"       "macroexpands to nil"  {:label "false"}]
             ["macroexpands to ..." :currently-loading?])

     (colors [A] [:currently-loading? "(process-after-load!)" {:label "true"}])
     (colors [B] [:currently-loading? "(when...)"             {:label "false"}])

     [:run-block! {:label "(run-execute-report! :block ns block)"}]
     (colors [B]
             ["(when...)" "(store-tests!)" {:label "(:store (config))"}]
             ["(when...)" :run-block!      {:label "(:run (config))"}])

     (colors [A]
             ["(process-after-load!)" "(process-tests-on-load-now!)"
              {:label "... and so on until load has run"}])

     (subgraph
       :cluster_around-clj-load-hook-after
       [{:label "around-clj-load-hook (after)"}

        [:store-*tests-to-process*! {:label "(run! #(apply store-tests! %) @*tests-to-process*)"}]
        [:run-suite!                {:label "(run-execute-report! :suite @*tests-to-process*)"}]

        (colors [A]
                ["(process-tests-on-load-now!)" :store-*tests-to-process*!
                 {:label "(when (:store (config)) ..."}]
                ["(process-tests-on-load-now!)" :run-suite!
                 {:label "(when (:run (config)) ..."}])])



     (subgraph :related-nodes
               [{:concentrate "true"}
                (edge-attrs {:color C :dir "back" :style "dashed"})

                [:setup-*tests-to-process* :store-*tests-to-process*!]
                [:setup-*tests-to-process* :run-suite!]
                [:currently-loading! :currently-loading?]
                [:clear-*tests* "(store-tests!)"]
                [:clear-*tests* :store-*tests-to-process*!]])

     [:run-suite! :suite]
     [:run-block! :suite]

     (subgraph :cluster_run-execute-report
               [{:label :run-execute-report!}

                [:suite                   :namespace]
                [:namespace               :block]
                [:block                   :case]
                [:case                    :for-each-executor]
                [:for-each-executor       :RunnerP/run-case]
                [:RunnerP/run-case        :ExecutorP/execute-case]])

     [:ExecutorP/execute-case  :lang?]
     [:lang? :CljExecutor/execute-case {:label "clj"}]
     [:lang? :CljsExecutor/execute-case {:label "cljs"}]
     [:CljExecutor/execute-case :run-test-and-yield-report!]]))

(defn -main [& _args]
  (let [d (dot code-paths)]
    (save! d "doc/code-paths.svg" {:format :svg})
    (show! d)))

