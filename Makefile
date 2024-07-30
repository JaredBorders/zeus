.PHONY: build run test fmt clean

build: fmt
	dune build

run: fmt
	dune exec bin/main.exe

test: fmt
	dune test

fmt:
	dune fmt

clean:
	dune clean
