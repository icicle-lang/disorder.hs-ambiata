MFLAGS =
MAKEFLAGS = $(MFLAGS)
SANDBOX = ../.cabal-sandbox
CABAL_FLAGS =
LIB = ../lib
DEPS = ${SANDBOX}/.cairn-lens

.PHONY: build test repl repl-test quick tags

default: repl

${SANDBOX} cabal.sandbox.config:
	cabal sandbox --sandbox ${SANDBOX} init

${DEPS}: ${SANDBOX} $(wildcard *.cabal) cabal.sandbox.config
	cabal install -j --only-dependencies --enable-tests
	cabal configure --enable-tests ${CABAL_FLAGS}
	touch $@

build: ${DEPS}
	cabal build

test: ${DEPS}
	cabal test --log=/dev/stdout

repl: ${DEPS}
	cabal repl

repl-test: ${DEPS}
	cabal repl test

quick: ${DEPS}
	ghci -package-db=$(wildcard ${SANDBOX}/*-packages.conf.d) -isrc -itest test/test.hs

tags:
	hasktags -e src test main
