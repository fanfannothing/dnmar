#!/bin/bash

export JAVA_OPTS="-Xmx2500M -Xms2500M"
sbt "run --trainProto ../data/train-Multiple.pb.gz --testProto ../data/test-Multiple.pb.gz"
