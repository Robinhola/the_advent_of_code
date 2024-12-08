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

