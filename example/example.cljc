(ns example
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [clojure.core.async :refer [chan >! go go-loop <! timeout close!]]
            [missionary.core :as m]))

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

#?(:clj
   (tests
     "async tests"
     (rcf/set-timeout! 100)
     (future
       (rcf/! 1)
       (Thread/sleep 10)
       (rcf/! 2)
       (Thread/sleep 200) ; timeout
       (rcf/! 3))
     % := 1
     % := 2
     % := ::rcf/timeout ; fail, timeout
     ))

#?(:cljs
   (tests
    "async tests"
    (rcf/set-timeout! 100)
    (js/setTimeout
     (fn []
       (rcf/! 1)
       (js/setTimeout
        (fn []
          (rcf/! 2)
          (js/setTimeout
           (fn []
             (rcf/! 3))
           200))
        10))
     0)
    % := 1
    % := 2
    % := ::rcf/timeout ; fail, timeout
    ))

(tests
  "core.async"
  (def c (chan))
  (rcf/set-timeout! 100)
  (go-loop [x (<! c)]
    (when x
      (<! (timeout 10))
      (! x)
      (recur (<! c))))
  (go (>! c :hello) (>! c :world))
  % := :hello ; queue
  % := :world ; queue
  (close! c) ; dispose when queue is empty
  )

(tests
  "missionary"
  (def !x (atom 0))
  (def dispose ((m/reactor (m/stream! (m/ap (! (inc (m/?< (m/watch !x)))))))
                (fn [_] (prn :done)) #(prn :fail %)))
  % := 1
  (swap! !x inc)
  (swap! !x inc)
  % := 2
  % := 3
  (dispose))
