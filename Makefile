HACKAGE = hangman-ascii


.PHONY: run
run:
	cabal v2-run $(HACKAGE)


.PHONY: build
build:
	cabal v2-build $(HACKAGE)


.PHONY: configure
configure:
	cabal v2-configure --enable-tests


.PHONY: test
test:
	cabal v2-test


.PHONY: install
install:
	-rm $(HOME)/.cabal/bin/$(HACKAGE)
	cabal v2-install $(HACKAGE)


.PHONY: env
env:
	cabal update
	cabal install


.PHONY: clean
clean::
	-cabal v2-clean
