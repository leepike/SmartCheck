# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
# please use tabs (8 width)

################################################################################
# Building

CABAL_INSTALL = cabal install

.PHONY: all
all: cabal-build

# Regular (default) build
cabal-build: .cabal-sandbox
	$(CABAL_INSTALL)

# Regression build, requires more packages in the sandbox
regression-build: .cabal-sandbox
	$(CABAL_INSTALL) -fregression-flag

.cabal-sandbox: $(MAKEFILE_LIST)
	cabal sandbox init

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

################################################################################
# Testing/comparing SmartCheck to other test frameworks

.PHONY: regression
regression: regression-build
	$(MAKE) all -C regression

.PHONY: regression-clean
regression-clean:
	$(MAKE) clean -C regression
