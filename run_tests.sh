#!/bin/bash

echo "Clojure"
clojure -X:test :dirs "[\"test\" #_\"example\"]" :patterns "[\"example.*\" \"hyperfiddle.*\"]"

# echo "Node"
# ./node_modules/.bin/shadow-cljs -A:cljs:test release :test
# node out/node-tests.js
# 
# echo "Browser"
# ./node_modules/.bin/shadow-cljs -A:cljs:test release :browser-test
# ./node_modules/.bin/karma start --single-run


