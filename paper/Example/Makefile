#TARGETS=qcNone.out qc10.out qc20.out feat.out sc.out
TARGETS=qcNone.out #qc10.out feat.out sc.out
LOG=init.log
RNDS=100
SRC=InitEx2

all : $(TARGETS)

%.out : $(SRC).hs
	ghc --make -O2 -D$* -o $@ $<
	echo "*******************" >> $(LOG)
	echo $@ >> $(LOG)
	./$@ $(RNDS)

.PHONY : clean
clean :
	rm -f $(TARGETS)
	rm -f $(SRC).o $(SRC).hi

.PHONY : veryclean
veryclean : clean
	rm -f $(LOG)
