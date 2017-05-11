#!/usr/bin/python

# Parses RLWE challenge/instance files:
# takes the path to a .challenge file first, and an optional second path to a
# corresponding .instance file. Prints the contents of the .challenge file
# and the .instance file, if a path is provided.
# See comments in the code for examples of how to access message contents.
#
# You will need the "protobuf" python package to run this code, which you can
# install with "easy_install protobuf"

# You can find more detailed information about parsing messages in python here:
# https://developers.google.com/protocol-buffers/docs/reference/python-generated
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

if __name__ == "__main__":

  if len(sys.argv) < 2 or len(sys.argv) > 3:
    print "Usage:", sys.argv[0], "path/to/.challenge [path/to/.instance]"
    sys.exit(-1)

  challenge = parse_challenge(sys.argv[1])

  if challenge is None:
    print "Could not parse the challenge parameters."
    sys.exit(-1)
  print challenge # print all contents
  # examples of how to access message contents
'''
  print challenge.numInstances # access a field
  print challenge.WhichOneof("params") # shows which type of parameter the message contains
  print {'cparams': challenge.cparams.m,
         'dparams': challenge.dparams.numSamples,
         'rparams': challenge.rparams.q}[challenge.WhichOneof("params")]
'''
  if len(sys.argv) == 3:
    instance = parse_instance(challenge, sys.argv[2])
    if instance is None:
      print "Could not parse instance."
      sys.exit(-1)
    print instance # print all contents
    # it is easy to access any member of the parsed message
    #print instance.samples[0].a.xs[0]

  sys.exit(0)