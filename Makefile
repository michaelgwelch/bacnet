
test:
	cabal test
	cat dist/test/bacnet-encoding-0.1.0.0-tests.log

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

.PHONY: test
.PHONY: configure
.PHONY: confgiureTest
.PHONY: lint
.PHONY: clean
.PHONY: dump-imports
