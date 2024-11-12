# Welcome to the advent of code

https://adventofcode.com

## Follow the following convention

Folders are named as follows:
- YYYY_DD_{coding_language}

Executables read from stdin and output the solution to stdout

Add Makefile to your folders so that the following works:

```sh
cd YYYY_DD_{coding_language}
cat input | make run
echo "line1\nline2" | make run
```

## Use bootstrap to start with a mini ocaml or python project

```sh
python3 ./bootstrap.py
```

## Use goto.sh to navigate quickly between days

```sh
./goto.sh
```

## And you may use today-ocaml or today-python as shortcuts

```sh
./today_ocaml.sh
./today_python.sh
```

## Have fun!
