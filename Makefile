
test:
	cabal test

configure:
	cabal configure

configureTest:
	cabal configure --enable-tests

lint:
	hlint .

clean:
	cabal clean

dump-imports:
	cabal build --ghc-option=-ddump-minimal-imports

docs:
	cabal haddock --hyperlink-source

deps:
	cabal build tests --enable-tests --only-dependencies

.PHONY: test
.PHONY: configure
.PHONY: confgiureTest
.PHONY: lint
.PHONY: clean
.PHONY: dump-imports
