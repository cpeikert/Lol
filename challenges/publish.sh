#!/bin/sh

execname=reveal
pubdir=publish

mkdir -p $pubdir

echo "Building challenge publisher..."
cabal build $execname

echo "Running challenge publisher..."
./dist/build/$execname/$execname

echo "Tarring public secrets..."
tar czf $pubdir/secret_files.tar.gz $( find -P challenge-files -name "*.secret" -or -name "*.xml" -or -name "beacon.cer" )