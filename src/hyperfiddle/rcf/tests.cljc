(ns hyperfiddle.rcf.tests
  (:require [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.rcf.unify :as u]))

(tests
  (u/unify 1 '_) := '{_ [1]})

(tests
  "Basics"
  (u/unify 1 1) := {}
  (u/unify 1 2) := ::u/fail
  (u/unify 1 '_) := '{_ [1]}
  (u/unify 1 '?a) := '{?a 1}
  (u/unify [1 2 3] '[_ _ ?a]) := '{_ [1 2], ?a 3}

  "Both sides"
  (u/unify '[?a 1] '[?a ?a]) := '{?a 1}

  "Cycle"
  (u/unify '[?a ?b] '[?b ?a]) := '{?a ?b, ?b ?a}

  "Transitive"
  (u/unify '[?a ?b 1] '[?b _ ?a]) := '{?a ?b, _ [?b], ?b 1})

(tests
  "Ground"
  "Cycle"
  (u/ground (u/unify '[?a ?b] '[?b ?a])) := '{?a ::u/cycle, ?b ::u/cycle}

  "Transitive"
  (u/ground (u/unify '[?a ?b 1] '[?b _ ?a])) := '{?a 1, _ [1], ?b 1}
  )


(tests
  "Composite"
  (u/unify {:first '?first  :last '?last    :genre :giallo}
           {:first "Dario"  :last "Argento" :genre :giallo}) := '{?first "Dario", ?last "Argento"}
  (u/unify '[(?a * ?x | 2) + (?b * ?x) + ?c]
           '[?z + (4 * 5) + 3])                              := '{?c 3, ?x 5, ?b 4, ?z (?a * ?x | 2)}
  (u/unify '[(?a * ?x | 2) + (?b * ?x) + ?c]
           '[(?a * 5 | 2) + (4 * 5) + 3])                    := '{?c 3, ?b 4, ?x 5}
  (u/unify '[(?a * 5 | 2) + (4 * 5) + 3]
           '[?z + (4 * 5) + 3])                              := '{?z (?a * 5 | 2)}
  (u/unify '[?a ?a] [1 2])                                   := '{?a ::u/fail}

  "Spread"
  (u/unify [1 2 3] '[?x & ?more])                            := '{?more (2 3), ?x 1}
  (u/unify [1 2 3] '[_ _ _ & ?more])                         := {_ [1 2 3]}
  (u/unify [1 2 3 4 5] '[_ _ _ & ?more])                     := '{_ [1 2 3] ?more (4 5)}
  (u/unify [1 2 3 4 5] '[_ ?b _ & ?more])                    := '{_ [1 3], ?more (4 5), ?b 2}
  (u/unify [:foo 1 2] '[?head & _])                          := '{?head :foo, _ [(1 2)]}
  )

(tests
  (u/unifier ['?first "Argento"]
             ["Dario" '?last])             := ["Dario" "Argento"]

  (u/unifier '[(?a * ?x | 2) + (?b * ?x) + ?c]
             '[?z + (4 * 5) + 3])          := '[(?a * 5 | 2) + (4 * 5) + 3]

  (u/unifier '{?a 1 :b :a} '{?b 1 :b ?a})  := {:a 1, :b :a}
  (u/unifier {:a 1, :b 2} '{?a ?b, ?b ?a}) := {::u/fail ::u/fail}
  (u/unifier '[?a ?b] '[?b ?a])            := [::u/cycle ::u/cycle]
  )

;; (hyperfiddle.rcf/enable!)
