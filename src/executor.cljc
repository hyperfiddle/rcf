(declare run-test-and-yield-report!)

(defprotocol ExecutorP
  (before-execute-suite     [this ns->tests])
  (before-execute-namespace [this ns tests])
  (before-execute-block     [this ns tests])
  (before-execute-case             [this ns case])
  (execute-case             [this ns case])
  (after-execute-case       [this ns report])
  (after-execute-block      [this ns reports])
  (after-execute-namespace  [this ns reports])
  (after-execute-suite      [this ns->reports]))


(defrecord CljExecutor [opts store]
  ExecutorP
  (before-execute-suite     [this ns->tests])
  (before-execute-namespace [this ns tests])
  (before-execute-block     [this ns tests])
  (before-execute-case      [this ns tests])
  (execute-case             [this ns case] (run-test-and-yield-report! ns case))
  (after-execute-case       [this ns report])
  (after-execute-block      [this ns reports])
  (after-execute-namespace  [this ns reports])
  (after-execute-suite      [this ns->reports]))

;; TODO
(defrecord CljsExecutor [opts store]
  ExecutorP
  (before-execute-suite     [this ns->tests])
  (before-execute-namespace [this ns tests])
  (before-execute-block     [this ns tests])
  (execute-case             [this ns case])
  (after-execute-case       [this ns report])
  (after-execute-block      [this ns reports])
  (after-execute-namespace  [this ns reports])
  (after-execute-suite      [this ns->reports]))
