(ns hyperfiddle.rcf.example-test
  (:require [clojure.core.async :refer [chan >! go go-loop <! timeout close!]]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(rcf/enable!)

(tests
  "equality"
  (inc 1) := 2)

(tests
 "nested tests"
 (tests
  "are sometimes convenient"
  1 := 1))

(tests
 "Macros are supported"
 (with-out-str 
   "print is captured"
   (print "hello")
   1 := 1)
 := "hello")

(tests
 "let body"
 (let [a 1, b 2] [a b] := [1 2]))

(tests
 "def in same form (Gilardi scenario)"
 (def a 1)
 a := 1

 "inline def"
 ((def b identity) 1) := 1)

(tests
 ((fn [a] a := 1) 1) := 1)

(tests
 "REPL bindings work"
 (inc 1) := 2
 (dec *1) := 1
 
 (tests 1 2 3 *3 := 1, *2 := 2, *1 := 3))

(tests
 "wildcards"
 {:a :b, :b [2 :b]} := {:a _, _ [2 _]})

(tests
 "unification"
 {:a :b, :b [2 :b]} := {:a ?b, ?b [2 ?b]})

(tests
 "unification on reference types"
 (def x (atom nil))
 {:a x, :b x} := {:a ?x, :b ?x})

(tests
 "Async queue"
 (tap 1) % := 1
 (future (Thread/sleep 300) (tap 2))
 % := 2
 )

(tests
 (tests (future (Thread/sleep 100) (tap :a) :b) %)
 := :a)

(tests
  ":throws"
  (let [e (ex-info "" {})]
    (throw e) :throws clojure.lang.ExceptionInfo))

(tests
 "missionary"
 (def !x (atom 0))
 (def dispose ((m/reactor (m/stream! (m/ap (tap (inc (m/?< (m/watch !x)))))))
               (fn [_] (prn ::done)) #(prn ::crash %)))
 % := 1
 (swap! !x inc)
 (swap! !x inc)
 % := 2
 % := 3
 (dispose))


(tests
 "core.async"
 (def c (chan))
 (go-loop [x (<! c)]
   (when x
     (<! (timeout 10))
     (tap x)
     (recur (<! c))))
 (go  (>! c :hello) (>! c :world))
 % := :hello
 % := :world
 (close! c))

(tests
  (def task (fn [success! failure!] (success! 1) (fn cancel [] (tap ::dispose))))
  (with (task tap tap)
    % := 1)
  % := ::dispose)
