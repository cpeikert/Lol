#!/bin/sh

pubdir=publish
filename=challenge_files
execname=challenge
privkey=lolchallenge.key # make sure this matches the value in makeSignKey

mkdir -p $pubdir

echo "Building challenge generator..."
cabal build $execname

echo "Running challenge generator..."
./dist/build/$execname/$execname

echo "Tarring challenge files..."
tar czf $pubdir/$filename.tar.gz $( find -P challenge-files -name "*.instance" -or -name "revealData.txt" )

echo "Hashing tarball..."
# originstamp supports max of 64 hex characters, so use SHA256
openssl dgst -sha256 -hex $pubdir/$filename.tar.gz

echo "Signing hash..."
openssl dgst -sha256 -sign ~/.ssh/lolchallenge.key -out $pubdir/$filename.sig $pubdir/$filename.tar.gz

echo "Now go to www.originstamp.org to commit to the hash."