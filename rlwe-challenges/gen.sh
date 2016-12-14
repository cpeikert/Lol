#!/bin/sh

execname=rlwe-challenges
tarfile=rlwe-challenges.tar.gz
challDir=rlwe-challenges

echo "Running challenge generator..."
./dist/build/$execname/$execname generate # --init-beacon=1378395540
rc=$?
if [ $rc -ne 0 ]
then exit
fi;

echo "Tarring challenge files..."
tar czf $tarfile $( find -P $challDir -name "*.instance" -or -name "*.challenge" )

echo "Signing the tar file..."

# run "gpg --fingerprint"
# > ...
# > pub   4096R/B8B245F5 2016-04-12 [expires: 2020-04-11]
# >       Key fingerprint = 8126 1E02 FC1A 11C9 631A  65BE B5B3 1682 B8B2 45F5
# > uid       Chris Peikert (Signing key for Ring-LWE challenges) <cpeikert@alum.mit.edu>

# put 0xB8242E6B below (not sure if this is the same across multiple computers)
gpg -u 0xB8242E6B --yes -s $tarfile

echo "Hashing the tar file..."
openssl dgst -sha256 $tarfile

echo "Go to www.originstamp.org to commit to the signature."
