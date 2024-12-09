.PHONY: all deps

default: all

deps:
	cd ./advent; make -s;

all: deps
	@for i in $$(find . -type d -depth 1 | grep -e ocaml | sort); \
		do ( \
		cd $$i; make -s; \
		cd - > /dev/null; \
		echo ">\t  $$i" \
		) & done; \
		wait

clean:
	@for i in $$(find . -type d -depth 1 | grep -e ocaml | sort); \
		do ( \
		cd $$i/advent; dune clean; \
		cd - > /dev/null; \
		echo ">\t  $$i" \
		) & done; \
		wait

run:
	@for i in $$(find . -type d -depth 1 | grep -e ocaml | grep -v default | sort); \
		do ( \
		echo "Running $$i"; \
		cd $$i; make with_input -s; \
		cd - > /dev/null; \
		echo ""; \
		); done; \
		wait

copy-main:
	for i in $$(find . -name main.ml | grep -v _build | grep -v default); do cp ocaml_default_project/advent/bin/main.ml $$i; done
