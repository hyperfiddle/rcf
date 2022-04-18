#!/bin/sh

echo "Running NodeJS tests"
./node_modules/.bin/shadow-cljs -A:cljs:test release :test
node out/node-tests.js