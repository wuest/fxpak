.PHONY: clean all dist
.DEFAULT_GOAL := all

all:
	cabal v2-build

dist:
	cabal v2-sdist

clean:
	rm -rf dist*
