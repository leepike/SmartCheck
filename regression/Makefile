DIRS       = List Div0 Heap PaperExample1 Parser
# DIRS       = Degenerate
BENCHMARKS = $(foreach dir, $(DIRS), $(dir).reg)
CLEANS     = $(foreach dir, $(DIRS), $(dir).clean)
VERYCLEANS = $(foreach dir, $(DIRS), $(dir).veryclean)

.PHONY: all clean veryclean

all: $(BENCHMARKS)

%.reg:
	$(MAKE) all -C $*

clean: $(CLEANS)

%.clean:
	$(MAKE) clean -C $*

veryclean: $(VERYCLEANS)

%.veryclean:
	$(MAKE) veryclean -C $*
