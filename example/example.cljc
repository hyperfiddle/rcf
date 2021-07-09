(ns example
  (:require [hyperfiddle.rcf :refer [tests ! %]]))

(defn get-extension [path]
  (let [found (last (re-find #"(\.[a-zA-Z0-9]+)$" path))
        ext (and found (subs found 1))]
    (or ext "")))

(tests
  "empty"
  (get-extension "") := ""
  (get-extension ".") := ""
  (get-extension "..") := ""
  (get-extension "image") := ""
  (get-extension "image.") := ""
  (get-extension "image..") := ""

  "found"
  (get-extension "image.png") := "png"
  (get-extension "image.blah.png") := "png"
  (get-extension "image.blah..png") := "png")

; features

(tests
  "equality"
  (inc 1) := 2

  "wildcards"
  {:a :b, :b [2 :b]} := {:a _, _ [2 _]}

  "unification"
  {:a :b, :b [2 :b]} := {:a ?b, ?b [2 ?b]}

  "unification on reference types"
  (def x (atom nil))
  {:a x, :b x} := {:a ?x, :b ?x}

  "the usual REPL bindings"
  :foo
  :bar
  :baz
  *3 := :foo
  *2 := :bar
  *1 := :baz

  (tests
    "nested tests for convenience"
    1 := 1)

  "inequality"
  1 :<> 2

  "async tests"
  (set! rcf/*timeout* 100)
  (future
    (rcf/! 1)
    (Thread/sleep 10)
    (rcf/! 2)
    (Thread/sleep 200) ; timeout
    (rcf/! 3))
  % := 1
  % := 2
  % := 3 ; fail, timeout

  "core.async"
  (require '[clojure.core.async :refer [chan >! go <! timeout]])
  (def c (chan))
  (go (while true (<! (timeout 10)) (! (<! c))))
  (go (>! c :hello) (>! c :world))
  % := :hello ; queue
  % := :world ; queue
  (close! c) ; dispose when queue is empty

  "missionary"
  (require '[missionary.core :as m])
  (def !x (atom 0))
  (def dispose ((m/reactor (m/stream! (m/ap (! (inc (m/watch !x))))))
                #(prn :done %) #(prn :fail %)))
  % := 1
  (swap! !x inc)
  (swap! !x inc)
  % := 2
  % := 3
  (dispose))
