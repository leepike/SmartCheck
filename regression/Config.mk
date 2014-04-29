RNDS	= 100
LOG		= regression.log
DB		= ../../.cabal-sandbox/*-packages.conf.d

%.out : $(SRC).hs
	mkdir -p out
	ghc --make -package-db $(DB) -O2 -D$* -o out/$@ $< ../Test.hs
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
