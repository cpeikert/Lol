#!/usr/bin/python

# Parses RLWE secret files:
# takes the path to a .secret file and prints the contents.
#
# You will need the "protobuf" python package to run this code, which you can
# install with "easy_install protobuf"

import argparse
import sys

import Challenges_pb2

def parse_secret(secret_path):
  if secret_path is None:
    return
  secret = None
  with open(secret_path) as f:
    secret = Challenges_pb2.Secret()
    secret.ParseFromString(f.read())
  return secret

if __name__ == "__main__":

  if len(sys.argv) != 2:
    print "Usage:", sys.argv[0], "path/to/.secret"
    sys.exit(-1)

  secret = parse_secret(sys.argv[1])

  if secret is None:
    print "Could not parse secret."
    sys.exit(-1)
  else:
    print secret

  sys.exit(0)