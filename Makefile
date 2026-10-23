.PHONY: all test test-js clean format

all:
	dune build

test:
	dune build @runtest

test-js:
	dune build @runtest-js

clean:
	dune clean

format:
	dune build --auto-promote @fmt
