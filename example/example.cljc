(ns example
  (:require [clojure.core.async :refer [chan >! go go-loop <! timeout close!]]
            [clojure.test :as t]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
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


(tests
  "equality"
  (inc 1) := 2

  "wildcards"
  {:a :b, :b [2 :b]} := '{:a _, _ [2 _]}

  "unification"
  {:a :b, :b [2 :b]} := '{:a ?b, ?b [2 ?b]}

  "unification on reference types"
  (def x (atom nil))
  {:a x, :b x} := '{:a ?x, :b ?x}

  "REPL bindings work"
  (inc 1)
  := 2
  ;; (dec *1) := 1 ;; FIXME

  (tests
   "nested tests are sometimes convenient"
   1 := 1))

(tests
  (rcf/set-timeout! 100)
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
            (defn set-timeout [f ms] (js/setTimeout ms f))
            (rcf/! 1) (set-timeout 10 (fn []
                                       (rcf/! 2) (set-timeout 200 (fn []
                                                                   (rcf/! 3)))))
            % := 1
            % := 2
            % := ::rcf/timeout
            )))


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
  (close! c))

(tests
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
