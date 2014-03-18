BENCHMARKS = Div0.reg Heap.reg PaperExample1.reg

export RNDS = 2
export LOG	= regression.log
export DB   = ../../.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d


.PHONY: all
all: $(BENCHMARKS)

%.reg:
	$(MAKE) all -C $*
