(ns hyperfiddle.rcf.queue
  (:require [hyperfiddle.rcf.time :as time])
  (:import (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(defn queue [] (LinkedBlockingQueue.))

(defn get-queue [^LinkedBlockingQueue q]
  (map :value q))

(defn poll!
  ([^LinkedBlockingQueue q, start, timeout, timeout-value]
   (let [now (time/current-time)]
     (if (time/timeout? now start timeout)
       timeout-value
       (:value (.poll q (time/remaining now start timeout) TimeUnit/MILLISECONDS) timeout-value))))
  ([^LinkedBlockingQueue q, start, timeout, timeout-value, callback]
   ;; TODO leverage this arity for non-blocking poll? call callback in (cc/future …)?
   (callback (poll! q start timeout timeout-value))))

(defn poll-n! [q start timeout missing-value n callback]
  (assert (nat-int? n))
  (poll! q start timeout missing-value
         (fn [x]
           (if (= 1 n)
             (callback x)
             (poll-n! q start timeout missing-value (dec n) (partial callback x))))))

(defn offer! [^LinkedBlockingQueue q, val]
  (.offer q {:value val})
  val)
