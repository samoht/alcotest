.PHONY: all test clean

all:
	dune build

test:
	dune build @runtest

clean:
	dune clean

format:
	dune build --auto-promote @fmt
