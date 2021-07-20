(ns example
  (:require [hyperfiddle.rcf :refer [tests]]))

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
  )
