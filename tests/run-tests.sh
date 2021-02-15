#!/bin/sh

set -eux

ros use sbcl-bin
ros run -- --version

# Note: Assume that repository is checkout to workspace folder in previous step
dir=/root/.roswell/local-projects/target
sudo cp -R $GITHUB_WORKSPACE ${dir}
cd ${dir}
rove *.asd 2>&1 | tee /tmp/test.log
