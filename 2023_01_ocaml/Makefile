build:
	cd advent; dune build

run: build
	cd advent; dune exec advent

test: build
	cd advent; dune runtest --auto-promote

format:
	cd advent; dune fmt

with_input:
	cd advent; cat ../input | dune exec advent
