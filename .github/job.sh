#!/bin/bash

sbt -Dsbt.test.fork=true ++$SCALA212! mimaReportBinaryIssues test
sbt -Dsbt.test.fork=false ++$SCALA213! test

.github/teardown.sh
