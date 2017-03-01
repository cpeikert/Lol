# This file is only used for interactive development;
# it is not needed to build the library with cabal/stack.
# For interactive development, you shouldn't need to explicitly use this file;
# it is called from the ghci script included with this repository.

CXX      ?= g++
AR       ?= ar
CXXFLAGS ?= -Wall -O3 -std=c++11 -fPIC

CSRCPATH = Crypto/Lol/Cyclotomic/Tensor/CPP
OBJPATH = lib
LIBPATH = $(OBJPATH)
LIBNAME = libctensor
CD = $(shell pwd)

$(OBJPATH)/%.o: $(CSRCPATH)/%.cpp
	@$(CXX) $(CXXFLAGS) -c -o $@ $<

# this makes the dst directory, then creates all of the individual object files
# finally, it packages them together in a single dynamic object file, which makes loading
# from ghci easier: just run 'ghci Foo.hs libctensor.so'
# See GHC bug https://ghc.haskell.org/trac/ghc/ticket/9386

all: mkdst $(addprefix $(OBJPATH)/,common.o crt.o g.o l.o mul.o norm.o random.o zq.o)
	@$(AR) rcs $(LIBPATH)/$(LIBNAME).a $(OBJPATH)/*.o
	$(CXX) -shared $(CXXFLAGS) -o $(LIBPATH)/$(LIBNAME).so $(OBJPATH)/*.o
	@ln -sf $(CD)/$(LIBPATH)/$(LIBNAME).so $(CD)/$(LIBPATH)/$(LIBNAME).dylib

mkdst:
	@mkdir -p $(OBJPATH)
	@mkdir -p $(LIBPATH)

clean:
	rm -f ./$(OBJPATH)/*.o
	rm -f ./$(LIBPATH)/*.a
	rm -f ./$(LIBPATH)/*.so
	rm -f ./$(LIBPATH)/*.dylib
	# make sure that cabal re-links with the new lib
	rm -f ./dist/build/test-lol/test-lol
	rm -f ./dist/build/bench-lol/bench-lol

.PHONY: all clean

# EAC: Old comment, that I'm keeping around just in case it comes back to bite us:
#   The .a file and the .so file must have different names, or cabal configure
#   gets confused and reports that it can't find libctensor.