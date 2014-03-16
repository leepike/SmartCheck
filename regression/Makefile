# We never run small.out since it's not feasible.
TARGETS=qcNone.out qc10.out feat.out qcjh.out smart.out qcjhint.out
LOG=regression.log
RNDS=1
SRC=PaperExample1

all : $(TARGETS)

%.out : $(SRC).hs
	ghc --make -package-db ../.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d -O2 -D$* -o $@ $<
	date >> $(LOG)
	echo "*******************" >> $(LOG)
	echo $@ >> $(LOG)
	./$@ \"$*\" $(RNDS)

.PHONY : clean
clean :
	rm -f $(TARGETS)
	rm -f $(SRC).o $(SRC).hi
	rm -f plot.tex

.PHONY : veryclean
veryclean : clean
	rm -f $(LOG)

.PHONY : csv
csv :
	rm -f *.csv
