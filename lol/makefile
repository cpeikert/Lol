# This file is only used for development, it is not needed to build the library.

CXX      ?= g++
AR       ?= ar
CXXFLAGS ?= -Wall -O3 -std=c++11 -fPIC

CSRCPATH = Crypto/Lol/Cyclotomic/Tensor/CTensor
OBJPATH = dist/build/ctensor
# if you change (LIBPATH) or LIBNAME, you must update Setup.hs.
LIBPATH = $(OBJPATH)
LIBNAME = libctensor
CD = $(shell pwd)

all: makelib

mkdst:
	mkdir -p $(OBJPATH)
	mkdir -p $(LIBPATH)

$(OBJPATH)/%.o: $(CSRCPATH)/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# this makes the dst directory, then creates all of the individual object files
# finally, it packages them together in a single dynamic object file, which makes loading
# from ghci easier: just run 'ghci Foo.hs libctensor.so'
# See GHC bug https://ghc.haskell.org/trac/ghc/ticket/9386

# the .a file and the .so file must have different names, or cabal configure gets confused and reports
# that it can't find libctensor.
makelib: mkdst $(OBJPATH)/common.o $(OBJPATH)/crt.o $(OBJPATH)/g.o $(OBJPATH)/l.o $(OBJPATH)/mul.o $(OBJPATH)/norm.o $(OBJPATH)/random.o $(OBJPATH)/tensor.o
	$(AR) rcvs $(LIBPATH)/$(LIBNAME).a $(OBJPATH)/*.o
	$(CXX) -shared $(CXXFLAGS) -o $(LIBPATH)/$(LIBNAME).so $(OBJPATH)/*.o
	ln -sf $(CD)/$(LIBPATH)/$(LIBNAME).so $(CD)/$(LIBPATH)/$(LIBNAME).dylib

clean:
	rm -f ./$(OBJPATH)/*.o
	rm -f ./$(LIBPATH)/*.a
	rm -f ./$(LIBPATH)/*.so
	rm -f ./$(LIBPATH)/*.dylib
	# make sure that cabal re-links with the new lib
	rm -f ./dist/build/test-lol/test-lol
	rm -f ./dist/build/bench-lol/bench-lol

.PHONY: all clean
