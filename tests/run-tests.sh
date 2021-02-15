#!/bin/sh

set -eux

ros use sbcl-bin
ros run -- --version

cd $GITHUB_WORKSPACE
rove *.asd 2>&1 | tee /tmp/test.log
