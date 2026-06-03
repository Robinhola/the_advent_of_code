# Ocaml Advent of Code - 2022

Most of the code is located in [lib](lib/advent_2022) and my input files (every user gets a different one) inside the [input](input/advent_2022) folder.
The [bin](bin/main.ml) folder only contains the executable.

## Tests

I love [Ocaml testing](https://dev.realworldocaml.org/testing.html) so naturally, some files contains tests (for instance [day05](lib/advent_2022/day05.ml)).

To write tests in Ocaml, you can use the `let%expect_test` syntax. When you run the tests, everything printed to stdout during the execution of the tests will now be considered as `expected`. It's especially powerful because you do not need to write this `expected` part yourself. You can simply run the tests and promote the expected output.

## Run me?

Once you are [up-and-running](https://ocaml.org/docs/up-and-running), you can follow the steps below:

```sh
# To simply build 
dune build

# To build and run 
dune exec ocaml 

# To run the tests 
dune runtest 

# And finally, to promote new test result changes
dune promote
```