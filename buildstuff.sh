#!/bin/bash

echo -en "\n--------------------------------------------------------------------------\n\n"

#stack exec ghc -- -E -cpp \
stack exec ghc -- \
    lol-cpp/Crypto/Lol/Cyclotomic/Tensor/CPP.hs \
    lol-cpp/Crypto/Lol/Cyclotomic/Tensor/CPP/Backend.hs \
    lol-cpp/Crypto/Lol/Cyclotomic/Tensor/CPP/Extension.hs \
    lol-cpp/Crypto/Lol/Cyclotomic/Tensor/CPP/Instances.hs \
    \
    lol/Crypto/Lol/CRTrans.hs \
    \
    lol/Crypto/Lol/Types/FiniteField.hs \
    lol/Crypto/Lol/Types/ZPP.hs \
    lol/Crypto/Lol/Types/Unsafe/ZqBasic.hs \
    \
    lol/Crypto/Lol/Cyclotomic/Tensor.hs \
    lol/Crypto/Lol/Cyclotomic/UCyc.hs \
    lol/Crypto/Lol/Cyclotomic/CRTSentinel.hs \
    #lol/Crypto/Lol/Cyclotomic/Cyc.hs \
    #\
    #lol/Crypto/Lol/Cyclotomic/CycRep.hs \
2>&1 | head -n 50
