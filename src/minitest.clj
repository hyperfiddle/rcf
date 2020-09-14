(ns minitest
  (:refer-clojure :exclude [test])
  (:require
    [meander.epsilon :as m]
    [taoensso.timbre :refer [error]]))


(declare tests test!)
(def ^:dynamic *tests* (atom {}))

(defmacro tests [& body]
  ;; One fn per "tests" block
  (let [parsed (m/rewrite (vec body)
                          [] []
                          [!xs ... '=> ?v & ?more]
                          [[[!xs ...] ?v] & (m/cata ?more)])
        code (for [[forms expected] parsed]
               `(try
                  (doseq [form# '~forms]
                    (let [v# (eval form#)]
                      (set! *3 *2) (set! *2 *1) (set! *1 v#)))
                  (let [e# (eval '~expected)]
                    (if (= *1 e#)
                      (println '~'test '~'passed '~(last forms) '~'=> '~expected)
                      (error   '~'test '~'failed '~(last forms) '~'=> '~expected e#)))
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
