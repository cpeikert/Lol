
# generate the python parsers with:
#
# > cd lol
# > protoc Lol.proto RLWE.proto --python_out=../chall-python
# > cd ../rlwe-challenges
# > protoc Challenges.proto -I. -I../lol --python_out=../chall-python
#
# you'll also need the "protobuf" python package

import Challenges_pb2
import sys

chall = Challenges_pb2.Challenge()
f = open("../chall-id0000-rlwec-m256-q769-l3-short-toy.challenge", "rb")
chall.ParseFromString(f.read())
f.close()
print chall

inst = Challenges_pb2.InstanceCont1()
f = open("../chall-id0000-rlwec-m256-q769-l3-short-toy-00.instance", "rb")
inst.ParseFromString(f.read())
f.close()
print inst

secret = Challenges_pb2.Secret1()
f = open("../chall-id0000-rlwec-m256-q769-l3-short-toy-00.secret", "rb")
secret.ParseFromString(f.read())
f.close()
print secret
