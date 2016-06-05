# This file is only used for development, it is not needed to build the library.

CXX      ?= g++
AR       ?= ar
CXXFLAGS ?= -Wall -O3 -std=c++11 -fPIC

CSRCPATH = Crypto/Lol/Cyclotomic/Tensor/CTensor
OBJPATH = dist/build/lib
# if you change LIBPATH or LIBNAME, you must update Setup.hs.
STATICLIBPATH = dist/build/staticlib
DYNLIBPATH = dist/build/dynamiclib
LIBNAME = libctensor
CD = $(shell pwd)

all: makelib

mkdst:
	mkdir -p $(OBJPATH)
	mkdir -p $(STATICLIBPATH)
	mkdir -p $(DYNLIBPATH)

$(OBJPATH)/%.o: $(CSRCPATH)/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# this makes the dst directory, then creates all of the individual object files
# finally, it packages them together in a single dynamic object file, which makes loading
# from ghci easier: just run 'ghci Foo.hs libctensor.so'
# See GHC bug https://ghc.haskell.org/trac/ghc/ticket/9386

# the .a file and the .so file must have different names, or cabal configure gets confused and reports
# that it can't find libctensor.
makelib: mkdst $(OBJPATH)/crt.o $(OBJPATH)/g.o $(OBJPATH)/generalfuncs.o $(OBJPATH)/l.o $(OBJPATH)/mul.o $(OBJPATH)/norm.o $(OBJPATH)/random.o
	$(AR) rcvs $(STATICLIBPATH)/$(LIBNAME).a $(OBJPATH)/*.o
	$(CXX) -shared $(CXXFLAGS) -o $(DYNLIBPATH)/$(LIBNAME).so $(OBJPATH)/*.o
	ln -sf $(CD)/$(DYNLIBPATH)/$(LIBNAME).so $(CD)/$(DYNLIBPATH)/$(LIBNAME).dylib

clean:
	rm -f ./$(OBJPATH)/*.o
	rm -f ./$(STATICLIBPATH)/*.a
	rm -f ./$(DYNLIBPATH)/*.so
	rm -f ./$(DYNLIBPATH)/*.dylib
	# make sure that cabal re-links with the new lib
	rm -f ./dist/build/test-lol/test-lol
	rm -f ./dist/build/bench-lol/bench-lol

.PHONY: all clean
