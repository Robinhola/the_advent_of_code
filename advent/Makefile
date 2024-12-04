build:
	cd robin_advent_lib; dune build; opam install .

run: build
	cd robin_advent_lib; dune exec robin_advent_lib

with_input: ./input 
	cd robin_advent_lib; cat ../input | dune exec robin_advent_lib

test: build
	cd robin_advent_lib; dune runtest --auto-promote

format:
	cd robin_advent_lib; dune fmt

fmt: format
