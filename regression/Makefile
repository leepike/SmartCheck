BENCHMARKS = List.reg Div0.reg Heap.reg PaperExample1.reg Parser.reg

.PHONY: all
all: $(BENCHMARKS)

%.reg:
	$(MAKE) all -C $*

# .PHONY: clean
