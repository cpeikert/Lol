#!/usr/bin/python

# Parses RLWE challenge files:
# use --challenge=PATH [--instance=PATH] to print the contents of a .challenge
# file and an optional .instance file in the challenge
# use --secret=PATH to print the contents of a .secret file
#
# You will need the "protobuf" python package to run this code, which you can
# install with "easy_install protobuf"

import argparse
import sys

import Challenges_pb2

def parse_challenge(challenge_path):
  if challenge_path is None:
    return
  challenge = None
  with open(challenge_path) as f:
    challenge = Challenges_pb2.Challenge()
    challenge.ParseFromString(f.read())
  return challenge

def parse_instance(challenge, instance_path):
  if challenge is None or instance_path is None:
    return
  instance = None

  with open(instance_path) as f:
    if challenge.cparams is not None:
      instance = Challenges_pb2.InstanceCont()
    elif challenge.dparams is not None:
      instance = Challenges_pb2.InstanceDisc()
    elif challenge.rparams is not None:
      instance = Challenges_pb2.InstanceRLWR()
    else:
      return
    instance.ParseFromString(f.read())
  return instance

def parse_secret(secret_path):
  if secret_path is None:
    return
  secret = None
  with open(secret_path) as f:
    secret = Challenges_pb2.Secret()
    secret.ParseFromString(f.read())
  return secret

if __name__ == "__main__":

  parser = argparse.ArgumentParser(description="Print some Lol protos.",
    epilog="Either a challenge or a secret must be provided but not both. If an " \
    "instance is provided, the corresponding challenge must be provided.")
  parser.add_argument("--challenge", help="Path to a .challenge file.")
  parser.add_argument("--instance", help="Path to a .instance file for the provided .challenge file.")
  parser.add_argument("--secret", help="Path to a .secret file.")

  args = parser.parse_args()
  challenge = parse_challenge(args.challenge)
  if challenge is None and args.instance is not None:
    print "You must provide a valid challenge in order to read the instance."
    sys.exit(0)
  instance = parse_instance(challenge, args.instance)

  secret = parse_secret(args.secret)

  if challenge is not None:
    print challenge
  if instance is not None:
    print instance
  if secret is not None:
    print secret

  sys.exit(0)

if len(sys.argv) != 2:
  print "Usage:", sys.argv[0], "ADDRESS_BOOK_FILE"
  sys.exit(-1)

secret = Challenges_pb2.Secret1()
f = open("../chall-id0000-rlwec-m256-q769-l3-short-toy-00.secret", "rb")
secret.ParseFromString(f.read())
f.close()
print secret
