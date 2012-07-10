#!/bin/bash

#export JAVA_OPTS="-Xmx1800M -Xms1800M"
export JAVA_OPTS="-Xmx1500M -Xms1500M"
sbt "run --trainProto ../data/train-Multiple.pb.gz --testProto ../data/test-Multiple.pb.gz"
