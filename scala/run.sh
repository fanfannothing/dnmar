#!/bin/bash

export JAVA_OPTS="-Xmx2000M -Xms2000M"
sbt "run --trainProto ../data/train-Multiple.pb.gz --testProto ../data/test-Multiple.pb.gz"
