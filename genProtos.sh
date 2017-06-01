#!/bin/bash

cd lol
hprotoc Lol.proto
hprotoc RLWE.proto
cd ../rlwe-challenges
hprotoc Challenges.proto -I. -I../lol