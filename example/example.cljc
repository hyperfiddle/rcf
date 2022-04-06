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

  (tests
    "nested tests (is there a strong use case?)"
    1 := 1

    #_#_"tests form returns final result"                   ; failing in CI? Works at the JVM REPL. What should this do in CLJS?
    (tests (inc 1) := 2 (inc *1)) := 3)

  (tests
    "REPL bindings work"
    (keyword "a") := :a
    (keyword "b") := :b
    (keyword "c") := :c
    *1 := :c
    *2 := :b
    *3 := :a
    *1 := :c                                                  ; inspecting history does not affect history

    (keyword "d") := :d
    *1 := :d
    *2 := :c
    *3 := :b
    (symbol *2) := 'c                                         ; this does affect history
    (symbol *2) := 'd))

(tests {:timeout 100}
  "async tests"
  #?(:clj  (tests
             (future
               (rcf/! 1) (Thread/sleep 10)
               (rcf/! 2) (Thread/sleep 200)
               (rcf/! 3))
             % := 1
             % := 2
             % := ::rcf/timeout)
     :cljs (tests
             (defn setTimeout [f ms] (js/setTimeout ms f))
             (rcf/! 1) (setTimeout 10 (fn []
             (rcf/! 2) (setTimeout 200 (fn []
             (rcf/! 3)))))
             % := 1
             % := 2
             % := ::rcf/timeout)))

(tests
  "core.async"
  (def c (chan))
  (go-loop [x (<! c)]
    (when x
      (<! (timeout 10))
      (! x)
      (recur (<! c))))
  (go (>! c :hello) (>! c :world))
  % := :hello
  % := :world
  (close! c)

  "missionary"
  (def !x (atom 0))
  (def dispose ((m/reactor (m/stream! (m/ap (! (inc (m/?< (m/watch !x)))))))
                (fn [_] #_(prn ::done)) #(prn ::crash %)))
  % := 1
  (swap! !x inc)
  (swap! !x inc)
  % := 2
  % := 3
  (dispose))
