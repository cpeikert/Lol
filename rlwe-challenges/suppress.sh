#!/bin/sh

execname=rlwe-challenges
challDir=rlwe-challenges

echo "Running secret suppresser..."
./dist/build/$execname/$execname suppress
rc=$?
if [ $rc -eq 0 ]
then
  echo "Tarring revealed secrets..."
  tar czf rlwe-secrets.tar.gz $( find -P $challDir -name "*.secret" -or -name "*.xml" -or -name "beacon.cer" )
fi;
