LOG					= regression.log
RNDS				= 1
DB          = ../../.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d
SRC         = Div0

# We never run small.out since it's not feasible.
TARGETS			= qc.out smart.out

all : $(TARGETS)

%.out : $(SRC).hs
	mkdir -p out
	ghc --make -package-db $(DB) -O2 -D$* -o out/$@ $< ../Test.hs
	date >> $(LOG)
	echo "*******************" >> $(LOG)
	echo $@ >> $(LOG)
	./out/$@ \"out/$*\" $(RNDS)

# %.pdf : $(TARGETS)
# 	gnuplot script.gnp

.PHONY : clean
clean :
	-rm -r out
	-rm $(SRC).o $(SRC).hi

.PHONY : veryclean
veryclean : clean
	-rm $(LOG)
