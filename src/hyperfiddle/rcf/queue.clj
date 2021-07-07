(ns hyperfiddle.rcf.queue
  (:import (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(defn queue [] (LinkedBlockingQueue.))

(defn poll!
  ([^LinkedBlockingQueue q, timeout, timeout-value]
   (:value (.poll q timeout TimeUnit/MILLISECONDS) timeout-value))
  ([^LinkedBlockingQueue q, timeout, timeout-value, callback]
   (callback (poll! q timeout timeout-value))))

(defn offer! [^LinkedBlockingQueue q, val]
  (.offer q {:value val})
  val)
