LOG					= regression.log
RNDS				= 1
DB          = ../../.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d
SRC         = PaperExample1
# We never run small.out since it's not feasible.
TARGETS			= qcNone.out

              # qc10.out		\
              # feat.out		\
              # qcjh.out		\
              # smart.out		\
              # qcjhint.out

all : $(TARGETS)

%.out : PaperExample1.hs
	mkdir -p out
	ghc --make -package-db $(DB) -O2 -D$* -o out/$@ $<
	date >> $(LOG)
	echo "*******************" >> $(LOG)
	echo $@ >> $(LOG)
	./out/$@ \"out/$*\" $(RNDS)

.PHONY : clean
clean :
	-rm -r out
	-rm $(SRC).o $(SRC).hi

.PHONY : veryclean
veryclean : clean
	-rm $(LOG)
