CXX      ?= g++
CD = $(shell pwd)
AUTOGENS = Lol.pb.o RLWE.pb.o Challenges.pb.o

%.o: %.cc %.h
	$(CXX) -c -o $@ $<

all: challInstParser secretParser

challInstParser: $(AUTOGENS) ChallInstParser.o
	$(CXX) -o $@ $^ -lprotobuf

secretParser: $(AUTOGENS) SecretParser.o
	$(CXX) -o $@ $^ -lprotobuf

mkdst:
	@mkdir -p $(OBJPATH)

clean:
	rm -f ./*.o
	rm -f ./*.so

.PHONY: challInstParser secretParser clean