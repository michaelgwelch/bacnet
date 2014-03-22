
test:
	cabal test
	cat dist/test/bacnet-encoding-0.1.0.0-tests.log

configure:
	cabal configure --enable-tests

lint:
	hlint .

.PHONY: test
.PHONY: configure
.PHONY: lint
