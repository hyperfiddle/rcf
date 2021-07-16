(ns hyperfiddle.rcf.time)

(defn current-time []
  #?(:clj (System/currentTimeMillis)
     :cljs (js/Date.now)))

(defn timeout? [now start timeout]
  (> now (+ start timeout)))

(defn remaining [now start timeout]
  (- (+ start timeout) now))
