#!/bin/bash

rm -rf "$HOME/.ivy2/local"
find $HOME/Library/Caches/Coursier/v1        -name "ivydata-*.properties" -delete
find $HOME/.ivy2/cache                       -name "ivydata-*.properties" -delete
find $HOME/.cache/coursier/v1                -name "ivydata-*.properties" -delete
find $HOME/.sbt                              -name "*.lock"               -delete
