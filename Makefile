all: dependencies test

test:
	cabal test

dependencies: .cabal-sandbox
	cabal install --avoid-reinstalls --enable-tests

.cabal-sandbox:
	cabal sandbox init

.PHONY: all dependencies test
