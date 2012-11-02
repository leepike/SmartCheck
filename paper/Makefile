DIR=pdflatex
DOC=paper
BIB=$(DOC).bib
LATEX=pdflatex

FIGS=./Figs

all: $(DIR) $(DOC).tex $(BIB)
	$(LATEX) -output-directory $(DIR) $(DOC)
	bibtex $(DIR)/$(DOC).aux
	$(LATEX) -output-directory $(DIR) $(DOC)
	$(LATEX) -output-directory $(DIR) $(DOC)
	cp $(DIR)/$(DOC).pdf .

#	twopi -Tps $(FIGS)/architecture_sensors.dot > $(FIGS)/architecture_sensors.ps
#	ps2epsi $(FIGS)/architecture_sensors.ps $(FIGS)/architecture_sensors.epsi
#	epstopdf $(FIGS)/architecture_sensors.epsi

$(DIR):
	mkdir -p $(DIR)

.PHONY: clean
clean:
	rm -rf $(DIR)
