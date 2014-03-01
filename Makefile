# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
# please use tabs (8 width)

PACKAGES = quickcheck ./

.PHONY: all
all: cabal-build

cabal-build: .cabal-sandbox
	cabal install $(PACKAGES)

.cabal-sandbox: $(MAKEFILE_LIST)
	@cabal sandbox init
	@cabal sandbox add-source $(ALL_CABAL_PKGS)

sandbox-clean:
	rm -rf cabal.sandbox.config .cabal-sandbox
	find . -name "dist" | xargs rm -rf

clean:
	@echo "Clean in the top level does not remove your cabal sandbox."
	@echo "If you want to remove your cabal sandbox, use the 'sandbox-clean' target"

.PHONY: init
init:
	git submodule init
	git submodule update

