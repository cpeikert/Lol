CXX      ?= g++
AR       ?= ar
CXXFLAGS ?= -Wall -O3 -std=c++11 -fPIC

CSRCPATH = Crypto/Lol/Cyclotomic/Tensor/CTensor
CDSTPATH = dist/build/Crypto/Lol/Cyclotomic/Tensor/CTensor
LIBPATH = dist/build
LIBNAME = libctensor

all: makelib

mkdst:
	mkdir -p $(CDSTPATH)
	mkdir -p $(LIBPATH)

$(CDSTPATH)/%.o: $(CSRCPATH)/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# this makes the dst directory, then creates all of the individual object files
# finally, it packages them together in a single dynamic object file, which makes loading
# from ghci easier: just run 'ghci Foo.hs libctensor.so'
# See GHC bug https://ghc.haskell.org/trac/ghc/ticket/9386#comment:1

# the .a file and the .so file must have different names, or cabal configure gets confused and reports
# that it can't find libctensor.
makelib: mkdst $(CDSTPATH)/basic.o $(CDSTPATH)/crt.o $(CDSTPATH)/g.o $(CDSTPATH)/generalfuncs.o $(CDSTPATH)/l.o $(CDSTPATH)/norm.o $(CDSTPATH)/random.o $(CDSTPATH)/zq.o
	$(CXX) -shared $(CXXFLAGS) -o $(LIBPATH)/$(LIBNAME)_dyn.so $(CDSTPATH)/basic.o $(CDSTPATH)/complex.o $(CDSTPATH)/crt.o $(CDSTPATH)/g.o $(CDSTPATH)/generalfuncs.o $(CDSTPATH)/l.o $(CDSTPATH)/norm.o $(CDSTPATH)/random.o $(CDSTPATH)/zq.o
	$(AR) rcvs $(LIBPATH)/$(LIBNAME).a $(CDSTPATH)/basic.o $(CDSTPATH)/crt.o $(CDSTPATH)/g.o $(CDSTPATH)/generalfuncs.o $(CDSTPATH)/l.o $(CDSTPATH)/norm.o $(CDSTPATH)/random.o $(CDSTPATH)/zq.o

clean:
	rm -f ./$(CDSTPATH)/*.o
	rm -f ./$(LIBPATH)/*.a
	rm -f ./$(LIBPATH)/*.so

.PHONY: all clean
