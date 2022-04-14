.PHONY: run
run:
	cabal v2-run

.PHONY: build
build:
	cabal v2-build

.PHONY: configure
configure:
	cabal v2-configure --enable-tests

.PHONY: test
test:
	cabal v2-test

.PHONY: env
env:
	cabal update
	cabal install

.PHONY: clean
clean::
	-cabal v2-clean
