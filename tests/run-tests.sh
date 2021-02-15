#!/bin/sh

set -eux

ros use sbcl-bin
ros run -- --version

mkdir ~/common-lisp/
projectdir=~/common-lisp/target
cp -R $GITHUB_WORKSPACE ${projectdir}
cd ${projectdir}
~/.roswell/bin/rove *.asd 2>&1 | tee /tmp/test.log

# Note: In Clozure CL, terminating debug console finishes in errcode 0,
# so grep message to check if the test has actually run.
grep -E "tests? passed" /tmp/test.log
