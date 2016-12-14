#!/bin/bash

## NIST Randomness Beacon verification routine
## This code is slightly adapted from the script
## provided by Elliot Williams at hackaday.com/2014/12/19/nist-randomness-beacon/
## which is itself modified from code provided by Lawrence Bassham, NIST.

## The UNIX time that you'd like to test:
##
## whichRecord=1400878200

## --------------- Utility Functions ----------------

## Extracts specified record from xml file
getValue() {
 xmllint --xpath "string(//*[local-name()='$1'])" $2
}

## Converts little-endian to big-endian
byteReverse() {
 len=${#1}
 for((i=${len}-2; i>=0; i=i-2)) do
 rev="$rev${1:$i:2}"
 done
 echo ${rev}
}

cleanup() {
  rm beacon.bin
  rm beacon.sig
}

## ---------------- Get an arbitrary record -----------------
## echo "Downloading data for: ${whichRecord}"
## curl -s https://beacon.nist.gov/rest/record/${whichRecord} -o rec.xml
path=$1
rec=$2

## ------------- Pack data into correct format --------------
## echo
## echo "## Create a summary of all of the data, save as beacon.bin"

## Strangest choice of format ever!
## Version number (ascii text)
## Update frequency (4 bytes)
## Time Stamp (8 bytes)
## The HW RNG seedValue (64 bytes)
## The previous output value, does the chaining (64 bytes)
## Status code (4 bytes)

getValue version $rec > beacon.bin
printf "%.8x" `getValue frequency $rec` | xxd -r -p >> beacon.bin
printf "%.16x" `getValue timeStamp $rec` | xxd -r -p >> beacon.bin
getValue seedValue $rec | xxd -r -p >> beacon.bin
getValue previousOutputValue $rec | xxd -r -p >> beacon.bin
printf "%.8x" `getValue statusCode $rec` | xxd -r -p >> beacon.bin

## ------------------ Verify signature on data --------------------

## echo "## Verify that the signature and NIST's public key correctly SHA512 sign the data"

## Download Beacon's public key
## echo "Downloading Beacon's public key"
## curl -s https://beacon.nist.gov/certificate/beacon.cer -o beacon.cer


## Create a bytewise reversed version of the listed signature
## This is necessary b/c Beacon signs with Microsoft CryptoAPI which outputs
## the signature as little-endian instead of big-endian like many other tools
## This may change (personal communication) in a future revision of the Beacon
signature=`getValue signatureValue $rec`
byteReverse ${signature} | xxd -r -p > beacon.sig

## Pull public key out of certificate
/usr/bin/openssl x509 -pubkey -noout -in $path/beacon.cer > $path/beaconpubkey.pem
## Test signature / key on packed data
sigcheck=$( /usr/bin/openssl dgst -sha512 -verify $path/beaconpubkey.pem -signature beacon.sig beacon.bin )
## echo
## echo

if [ "$sigcheck" != "Verified OK" ]; then
  cleanup
  echo "Beacon signature did not verify!"
  exit 1
fi

## ------------------ Verify Signature -> Output and Chaining ------------
## echo "The following three values should match: "
## echo " a direct SHA512 of the extracted signature"
## echo " the reported output value"
## echo " next record's previous output value"
## echo

## Just print output value
## echo "Reported output value"
beaconoutput=$( getValue outputValue $rec )
## echo

## Now turn the signature into the output value: again SHA512
## echo "SHA512 of the signature"
hashofsig=$( getValue signatureValue $rec | xxd -r -p | openssl dgst -sha512 -binary | xxd -p -u -c 128 )

if [ "$beaconoutput" != "$hashofsig" ]; then
  cleanup
  echo "Beacon output value did not match hash of signature!"
  exit 1
fi

## Now test chaining
## Get next record
## echo "Downloading the next record"
## curl -s https://beacon.nist.gov/rest/record/next/${whichRecord} -o next.xml
## Make sure that this period's output shows up as next period's "previous output"
## echo "Next value's reported previous output (test of forward chaining)"
## getValue previousOutputValue next.xml
## echo
## echo

## --------------------- The End -----------------------------------------

## If this all worked, we've verified that the signature (plus NIST's key)
## sign the hash of the random number and its support info
## _and_ we've verified that the outputValue is derived from them,
## so we know that this output value is in the chain.

## If we run this on every entry in the chain, and all works out just fine,
## then we'd know all is well

cleanup
exit 0
