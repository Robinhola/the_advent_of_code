build:
	cd advent; dune build

run: build
	cd advent; dune exec advent

with_input: ./input
	cd advent; cat ../input | dune exec advent

test: build
	cd advent; dune runtest --auto-promote

format:
	cd advent; dune fmt

fmt: format

