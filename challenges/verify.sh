#!/bin/sh

execname=verify
pubdir=publish

mkdir -p $pubdir

echo "CJP says: I don't think this script should be responsible for building/installing the binaries; instead, stack/cabal should do it.  This script should just run the right binaries."

echo "Building challenge verifier..."
cabal build $execname

echo "Running challenge verifier..."
./dist/build/$execname/$execname

echo "Verifying beacon signatures..."
beacons=$( find ./challenge-files -name "*.xml" )

RED='\033[1;31m'
GREEN='\033[1;32m'
NC='\033[0m' # No color

for f in $beacons
do
  echo -n "\t"
  echo -n $f
  echo -n "..."
  ./beaconVerify.sh $f
  rc=$?
  if [ $rc -ne 0 ]
  then echo "${RED}FAILED${NC}"
  else echo "${GREEN}PASSED${NC}"
  fi;
done
