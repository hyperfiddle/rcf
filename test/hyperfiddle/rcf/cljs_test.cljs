(ns hyperfiddle.rcf.cljs-test
  (:require [clojure.core.async :refer [chan >! go go-loop <! timeout close!]]
            [hyperfiddle.rcf :as rcf :refer [tests tap %]]
            [missionary.core :as m]))

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
 (rcf/set-timeout! 100)
 "async tests"
 (tests
  (defn set-timeout [f ms] (js/setTimeout ms f))
  (rcf/tap 1) (set-timeout 10 (fn []
                              (rcf/tap 2) (set-timeout 200 (fn []
                                                           (rcf/tap 3)))))
  % := 1
  % := 2
  % := ::rcf/timeout))

(tests
 "core.async"
 (def c (chan))
 (go-loop [x (<! c)]
   (when x
     (<! (timeout 10))
     (tap x)
     (recur (<! c))))
 (go (>! c :hello) (>! c :world))
 % := :hello
 % := :world
 (close! c))

(tests
 "missionary"
 (def !x (atom 0))
 (def dispose ((m/reactor (m/stream! (m/ap (tap (inc (m/?< (m/watch !x)))))))
               (fn [_] #_(prn ::done)) #(prn ::crash %)))
 % := 1
 (swap! !x inc)
 (swap! !x inc)
 % := 2
 % := 3
 (dispose))

(tests
 "rcf#56: % across try/catch (the in-house CPS refused this; cloroutine sequences it)"
 (rcf/tap (ex-info "boom" {}))
 (try (throw %) (catch :default e (ex-message e))) := "boom")

(tests
 "rcf#56: % in a recur argument (the in-house CPS refused this)"
 (rcf/tap 1) (rcf/tap 2) (rcf/tap 3)
 (loop [acc 0, n 3]
   (if (zero? n) acc (recur (+ acc %) (dec n)))) := 6)

(tests
 "rcf#56: % in a letfn BODY (not a bound fn) sequences fine"
 (rcf/tap 42)
 (letfn [(double-it [n] (* 2 n))]
   (double-it %)) := 84)
