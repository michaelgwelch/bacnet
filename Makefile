
test:
	cabal test
	cat dist/test/bacnet-encoding-0.1.0.0-tests.log

configure:
	cabal configure

configureTest:
	cabal configure --enable-tests --disable-optimization

lint:
	hlint .

.PHONY: test
.PHONY: configure
.PHONY: confgiureTest
.PHONY: lint
