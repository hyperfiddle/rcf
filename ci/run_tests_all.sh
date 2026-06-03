#!/bin/bash

# Resolve sibling scripts by this script's own location (ci/), not the caller's CWD —
# they still inherit CWD = rcf/, where their `clojure` / `./node_modules` paths resolve.
d="$(dirname "$0")"
"$d/run_tests_jvm.sh"
"$d/run_tests_node.sh"
"$d/run_tests_browser.sh"