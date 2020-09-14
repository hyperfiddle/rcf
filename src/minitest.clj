(ns minitest
  (:refer-clojure :exclude [test])
  (:require [taoensso.timbre :refer [error]]))


(declare tests test!)
(def ^:dynamic *tests* (atom {}))

(def ^:private ->|      #(apply comp (reverse %&)))
(def ^:private as-thunk #(do `(fn [] ~%)))
(def ^:private as-quote #(do `'~%))

(defmacro tests [& body]
  (let [parsed (->> (partition 3 1 body)
                    (filter (->| second #{'=>}))
                    (map (juxt first last))) ;; test & expectation
        cases  (->> parsed
                    (map (juxt (->| first  as-quote) (->| first  as-thunk)
                               (->| second as-quote) (->| second as-thunk))))]
    `(do
       (swap! *tests* update (ns-name *ns*) concat ~(vec cases))
       ; if repl mode, just run them
       (test!))))                                             ; danger

(defn- run-test! [[test-form test-thunk expect-form expect-thunk]]
  (let [test-v   (test-thunk)
        _        (do (set! *3 *2) (set! *2 *1) (set! *1 test-v))
        expect-v (expect-thunk)]
    (if (= test-v expect-v)
      (println "test passed" test-form "=>" expect-v)
      (error   "test failed" test-form "=>" expect-v expect-form))))

(defn test!
  ([] (test! (ns-name *ns*)))
  ([ns]
   (run! run-test! (get @*tests* (ns-name ns)))))

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

(let [a 1]
  (tests a => 1))
