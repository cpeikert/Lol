#!/bin/sh

execname=reveal
pubdir=publish

mkdir -p $pubdir

echo "CJP says: I don't think this script should be responsible for building/installing the binaries; instead, stack/cabal should do it.  This script should just run the right binaries."

echo "Building challenge revealer..."
cabal build $execname

echo "Running challenge revealer..."
./dist/build/$execname/$execname

echo "Tarring revealed secrets..."
tar czf $pubdir/rlwe_secrets.tar.gz $( find -P challenge-files -name "*.secret" -or -name "*.xml" -or -name "beacon.cer" )
