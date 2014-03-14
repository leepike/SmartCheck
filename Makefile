# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
# please use tabs (8 width)

PACKAGES = quickcheck
CABAL_INSTALL = cabal install --force-reinstalls

.PHONY: all
all: cabal-build

cabal-build: .cabal-sandbox $(PACKAGES)
	$(CABAL_INSTALL)

.cabal-sandbox: $(MAKEFILE_LIST)
	cabal sandbox init

quickcheck: .cabal-sandbox
	cd $@; cabal sandbox init --sandbox ../.cabal-sandbox
	cd $@; 	$(CABAL_INSTALL)

sandbox-clean:
	rm -rf cabal.sandbox.config .cabal-sandbox
	find . -name "dist" | xargs rm -rf

clean:
	@echo "Clean in the top level does not remove your cabal sandbox."
	@echo "If you want to remove your cabal sandbox, use the 'sandbox-clean' target"

.PHONY: configure
configure:
	git submodule init
	git submodule update

