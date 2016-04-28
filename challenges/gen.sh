#!/bin/sh

pubdir=publish
filename=rlwe_challenges
execname=gen

mkdir -p $pubdir

echo "CJP says: I don't think this script should be responsible for building/installing the binaries; instead, stack/cabal should do it.  This script should just run the right binaries."

echo "Building challenge generator..."
cabal build $execname

echo "Running challenge generator..."
./dist/build/$execname/$execname

echo "Tarring challenge files..."
tar czf $pubdir/$filename.tar.gz $( find -P challenge-files -name "*.instance" -or -name "beaconTime.txt" )

echo "Now sign the challenge file, and go to www.originstamp.org to commit to it."
