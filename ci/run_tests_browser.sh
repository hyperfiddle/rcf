#!/bin/sh -x

echo "Running Browser tests"
clojure -M:test:cljs -m shadow.cljs.devtools.cli release :browser-test --force-spawn
./node_modules/.bin/karma start --single-run $@ # --browsers Chrome
