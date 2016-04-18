CXX      ?= g++
AR       ?= ar
CXXFLAGS ?= -Wall -O3 -std=c++11 -fPIC

CSRCPATH = Crypto/Lol/Cyclotomic/Tensor/CTensor
OBJPATH = dist/build/lib
LIBPATH = dist/build
LIBNAME = libctensor

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
makelib: mkdst $(OBJPATH)/basic.o $(OBJPATH)/crt.o $(OBJPATH)/g.o $(OBJPATH)/generalfuncs.o $(OBJPATH)/l.o $(OBJPATH)/norm.o $(OBJPATH)/random.o
	$(CXX) -shared $(CXXFLAGS) -o $(LIBPATH)/$(LIBNAME)_dyn.so $(OBJPATH)/*.o
	$(AR) rcvs $(LIBPATH)/$(LIBNAME).a $(OBJPATH)/*.o

clean:
	rm -f ./$(OBJPATH)/*.o
	rm -f ./$(LIBPATH)/*.a
	rm -f ./$(LIBPATH)/*.so

.PHONY: all clean
