#!/bin/bash -e

sbt -Dsbt.test.fork=true ++$SCALA212! mimaReportBinaryIssues scalafmtCheckAll headerCheck Test/headerCheck test whitesourceCheckPolicies doc
sbt -Dsbt.test.fork=false ++$SCALA213! test

.github/teardown.sh
