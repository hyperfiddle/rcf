(ns minitest
  (:refer-clojure :exclude [test])
  (:require [taoensso.timbre :refer [error]]))


(declare tests test!)
(def ^:dynamic *tests* (atom {}))

(defmacro tests [& body]
  ;; One fn per "tests" block
  (let [parsed (->> (partition 3 1 body)
                    (filter #(-> % second (= '=>)))
                    (map (juxt first #(nth % 2))))
        code (for [[form expected] parsed]
               `(try
                  (let [v# (eval '~form)]
                    (set! *3 *2) (set! *2 *1) (set! *1 v#))
                  (let [e# (eval '~expected)]
                    (if (= *1 e#)
                      (println '~'test '~'passed '~form '~'=> '~expected)
                      (error   '~'test '~'failed '~form '~'=> '~expected e#)))
                  (catch Exception e#
                    ;; continue
                    (error e#))))]
    `(do
       (swap! *tests* update (ns-name *ns*) (fnil conj []) (do ~@code))
       ; if repl mode, just run them
       (test!))))                                             ; danger

(defn test!
  ([] (test! (ns-name *ns*)))
  ([ns]
   (run! eval (get @*tests* (ns-name ns)))))

(tests
  1
  1
  1
  (inc 42) => 43
  (inc 3) => 4
  (inc 3) => 5
  nil => nil
  nil => true
  )

(tests 1 => 1)
(tests (count (get @*tests* (ns-name *ns*) )) => 2)
