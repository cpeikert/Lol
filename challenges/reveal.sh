#!/bin/sh

execname=reveal
pubdir=publish

mkdir -p $pubdir

echo "Building challenge revealer..."
cabal build $execname

echo "Running challenge revealer..."
./dist/build/$execname/$execname

echo "Tarring revealed secrets..."
tar czf $pubdir/rlwe_secrets.tar.gz $( find -P challenge-files -name "*.secret" -or -name "*.xml" -or -name "beacon.cer" )