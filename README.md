# Welcome to the advent of code

https://adventofcode.com

## Follow the following convention

Folders are named as follows:
- YYYY_DD_{coding_language}

Python and Ocaml executables read from stdin and output the solution to stdout. Cpp usually take a filename as first argument.

Add Makefile to your folders so that the following works:

```sh
cd YYYY_DD_{coding_language}
cat input | make run
echo "line1\nline2" | make run
```

## Use bootstrap to start with a mini cpp, ocaml or python project

```sh
python3 ./bootstrap.py
```

## And you may use today-{cpp,ocaml,python} as shortcuts

```sh
./today_cpp.sh
./today_ocaml.sh
./today_python.sh
```

## Have fun!
