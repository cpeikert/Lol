#!/bin/sh

execname=rlwe-challenges
challDir=rlwe-challenges

echo "Running challenge verifier..."
./dist/build/$execname/$execname verify

echo "Verifying beacon signatures..."
beacons=$( find $challDir -name "*.xml" )

RED='\033[1;31m'
GREEN='\033[1;32m'
NC='\033[0m' # No color

for f in $beacons
do
  echo -n "\t"
  echo -n $f
  echo -n "..."
  ./beaconVerify.sh $challDir $f
  rc=$?
  if [ $rc -ne 0 ]
  then echo "${RED}FAILED${NC}"
  else echo "${GREEN}PASSED${NC}"
  fi;
done
