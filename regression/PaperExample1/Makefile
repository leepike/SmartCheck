SRC         = PaperExample1

# We never run small.out since it's not feasible.
TARGETS			= qcNone.out  \
              qc10.out		\
              feat.out		\
              qcjh.out		\
              smart.out		\
              qcjhint.out

GRAPHS      = data.pdf time-big.pdf time-small.pdf

all : $(TARGETS)

%.out : $(SRC).hs
	mkdir -p out
	ghc --make -package-db $(DB) -O2 -D$* -o out/$@ $< ../Test.hs
	date >> $(LOG)
	echo "*******************" >> $(LOG)
	echo $@ >> $(LOG)
	./out/$@ \"out/$*\" $(RNDS)

%.pdf : $(TARGETS)
	gnuplot script.gnp

.PHONY : clean
clean :
	-rm -r out
	-rm $(SRC).o $(SRC).hi

.PHONY : veryclean
veryclean : clean
	-rm $(LOG)
