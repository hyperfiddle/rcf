#!/bin/sh

echo "Running JVM tests"
clojure -X:test :dirs "[\"test\" \"example\"]" :patterns "[\"example.*\" \"hyperfiddle.rcf.*-test\"]"