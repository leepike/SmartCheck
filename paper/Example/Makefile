TARGETS=qcNone.out qc10.out qc20.out feat.out qcjh.out sc.out
LOG=init.log
RNDS=2000
SRC=InitEx2

all : $(TARGETS)

%.out : $(SRC).hs
	ghc --make -O2 -D$* -o $@ $<
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
