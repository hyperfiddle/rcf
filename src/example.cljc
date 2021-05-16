(ns example
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]))

(tests "Syntax"
  "an infix assertion is converted to a prefix one"
  (rcf/rewrite-infix '(1 2 3)) := '((do 1) (do 2) (do 3))
  (rcf/rewrite-infix '(1 := 2)) := '((= 1 2))
  (rcf/rewrite-infix '(0 1 := 2 3)) := '((do 0) (= 1 2) (do 3)))

(tests "Usage:"
  "infix `:=` is equality assertion"
  1 := 1
  1 := 2
  "foo" := "foo"
  (inc 0) := (dec 2)

  "a prefix expression works as in Clojure"
  (= 1 1)  ; useful to eval at the repl
  (:= 1 1) ; works too

  (tests "unification"
    "is supported with `:=`"
    1 := ?a                           ; passes {?a 1}
    {:a 1, :b 2} := {:a ?a, :b ?b}    ; passes {?a 1, ?b 2}

    "this fails because 1 != 2"
    (rcf/unifies? {:a 1, :b 2} {:a ?a, :b ?a}) := false
    "works as infix too"
    (:= 1 ?a)

    "wildcard is supported with `_` and always unifies."
    {:a 1, :b 2} := {:a _, :b _})

  "*1, *2 and *3 are respectively bound to the last, penultimate and antepenultimate values."
  :foo
  :bar
  :baz
  *3 := :foo
  *2 := :bar
  *1 := :baz

  (rcf/with-config {:dots true}
    (tests "Theses will just print a single char: ✅ if they succeed"
      (:= 1 1)
      (:= 2 2))))


(deftype Foo [x])

(tests "References works with unification"
  (def a (Foo. 1))
  {:a a, :a2 a} := {:a ?a, :a2 ?a})

#_(tests
    "Pattern matching works as in core.match, using :matches?"
    ;; '{:a 1, :b 2} :matches? {:a _, :b _}
    ;; '(:a :b :c)   :matches? ([:a _ :c] :seq)
    "Hello" :matches? #"H.*"

    (tests
      "it also works in prefix position"
      (:matches? 1 1)
      (:matches? "foo" #".oo")))
