(ns minitest
  (:refer-clojure :exclude [test])
  (:require
    [meander.epsilon :as m]
    [taoensso.timbre :refer [error]]))


(declare tests test!)
(def ^:dynamic *tests* (atom {}))

(defmacro tests [& body]
  `(do
     (swap! *tests* assoc (ns-name *ns*) '~body)
     ; if repl mode, just run them
     (test!)))                                              ; danger

(defn test!
  ([] (test! (ns-name *ns*)))
  ([ns]
   (let [parsed (m/rewrite (vec (get @*tests* ns))
                  [] []
                  [!xs ... '=> ?v & ?more]
                  [[[!xs ...] ?v] & (m/cata ?more)])]
     (doseq [[forms expected] parsed]
       (try
         (doseq [form forms]
           (let [v (eval form)]
             (set! *3 *2) (set! *2 *1) (set! *1 v)))
         (let [e (eval expected)]
           (if (= *1 e)
             (println 'test 'passed (last forms) '=> expected)
             (error 'test 'failed (last forms) '=> expected e)))
         (catch Exception e
           ; continue
           (error e)))))))

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
