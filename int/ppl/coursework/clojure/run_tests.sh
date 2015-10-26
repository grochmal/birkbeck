#!/bin/sh

# Working outside the REPL forces clojure to load it's entire
# VM each time.  Instead, the functions are in a separate file
# (from the tests) and can be loaded into the REPL, this approach
# speeds up the development and testing process.  The provided
# tests are in the test.clj file and the functions in the cw.clj
# file.  This script tells the test file where to find the functions
# by exporting the CLASSPATH environment variable.

# Use this script instead of running
# $ clojure tests.clj
# directly from the command line.

echo "export CLASSPATH=.:\$CLASSPATH"
export CLASSPATH=.:$CLASSPATH
echo clojure tests.clj
clojure tests.clj

