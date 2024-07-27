.PHONY: build run test fmt clean

build:
	dune build

run:
	dune exec bin/main.exe

test:
	dune test

fmt:
	dune fmt

clean:
	dune clean
