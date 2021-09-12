#!/bin/bash

echo "Clojure"
clojure -X:test:example :dirs [\"example\"] :patterns [\"example.*\"]

echo "Node"
./node_modules/.bin/shadow-cljs release :test
node out/node-tests.js

echo "Browser"
./node_modules/.bin/shadow-cljs release :browser-test
./node_modules/.bin/karma start --single-run


