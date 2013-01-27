TARGETS=qcNone.out qc10.out qc20.out feat.out sc.out
LOG=init.log
REPS=100
SRC=InitEx

all : $(TARGETS)

%.out : $(SRC).hs
	ghc --make -O2 -D$* -o $@ $<
	echo "*******************" >> $(LOG)
	echo $@ >> $(LOG)
	./$@ $(REPS)

.PHONY : clean
clean :
	rm -f $(TARGETS)
	rm -f $(SRC).o $(SRC).hi
