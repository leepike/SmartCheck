DIRS       = List Div0 Heap PaperExample1 Parser
BENCHMARKS = $(foreach dir, $(DIRS), $(dir).reg)
CLEANS     = $(foreach dir, $(DIRS), $(dir).clean)

.PHONY: all clean

all: $(BENCHMARKS)

%.reg:
	$(MAKE) all -C $*

clean: $(CLEANS)

%.clean:
	$(MAKE) clean -C $*
