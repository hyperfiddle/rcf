(ns user
  (:require [hyperfiddle.rcf :as rcf]))

(rcf/with-config {:enabled false}
  ;; tests won't run at require time.
  (require 'hyperfiddle.rcf.tests))
