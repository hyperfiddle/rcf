(ns minitest.explain
  (:gen-class)
  (:require [dorothy.core :refer [dot node-attrs] :as dodo]
            [dorothy.jvm  :refer [render save! show!]]))


(def A :aquamarine)
(def B :lightpink)

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

(defn colors [cs & edges]
  (splice (for [e edges
                c cs
                :let [[src dst attrs] e]]
            [src dst (assoc attrs :color c)])))

(defn digraph [items]
  (dodo/digraph (compose-graph items)))

(defn subgraph [name items]
  (dodo/subgraph name (compose-graph items)))

(def code-paths
  (digraph
    [(node-attrs {:shape :box})

     [:clj-repl-require    {:label "user=> (require 'my-namespace)"             :color A}]
     [:java-jar            {:label "$ java -jar my-app.jar"                     :color A}]
     [:tests-from-clj-repl {:label "user=> (tests ...)"                         :color B}]
     [:tests-from-cider    {:label "M-x cider-eval-sexp-at-point on (tests...)" :color B}]

     (colors [A]
             [:clj-repl-require    "around-clj-load-hook (before)"]
             [:java-jar            "around-clj-load-hook (before)"]
             ["around-clj-load-hook (before)" :load {:label "*currently-loading* := true"}]
             [:load                           :eval])

     (colors [B]
             [:tests-from-clj-repl :eval]
             [:tests-from-cider    :eval])

     (colors [A B]
             [:eval                           "macroexpanding (tests...)"]
             ["macroexpanding (tests...)"     "(load-tests?)"]

             ["(load-tests?)"       "macroexpands to ..."  {:label "true"}]
             ["(load-tests?)"       "macroexpands to nil"  {:label "false"}]
             ["macroexpands to ..." :*currently-loading*])

     (colors [A] [:*currently-loading* "(process-after-load!)" {:label "true"}])
     (colors [B] [:*currently-loading* "(when...)"             {:label "false"}])

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

        "(process-tests-on-load-now!)"
        [:when2      {:label "(when ...)"}]
        [:run-suite! {:label "(run-execute-report! :suite @*tests-to-process*)"}]

        (colors [A]
                ["(process-tests-on-load-now!)" :when2]
                [:when2 "(run! #(apply store-tests! %) @*tests-to-process*)"
                 {:label "(:store (config))"}]
                [:when2 :run-suite!
                 {:label "(:run (config))"}])])]))

(defn -main [& _args]
  (let [d (dot code-paths)]
    (save! d "doc/code-paths.svg" {:format :svg})
    (show! d)))

