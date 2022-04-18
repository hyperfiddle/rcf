#!/bin/bash

npx shadow-cljs -d nrepl/nrepl:0.9.0-beta4 -d cider/cider-nrepl:0.27.2 -A:dev:test:cljs server
