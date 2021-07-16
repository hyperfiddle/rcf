(ns hyperfiddle.rcf.queue
  (:require [hyperfiddle.rcf.time :as time])
  (:import (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(defn queue [] (LinkedBlockingQueue.))

(defn poll!
  ([^LinkedBlockingQueue q, start, timeout, timeout-value]
   (let [now (time/current-time)]
     (if (time/timeout? now start timeout)
       timeout-value
       (:value (.poll q (time/remaining now start timeout) TimeUnit/MILLISECONDS) timeout-value))))
  ([^LinkedBlockingQueue q, start, timeout, timeout-value, callback]
   (callback (poll! q start timeout timeout-value))))

(defn offer! [^LinkedBlockingQueue q, val]
  (.offer q {:value val})
  val)
