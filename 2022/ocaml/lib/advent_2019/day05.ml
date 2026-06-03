open! Base
open! Core
open! Import

(*
--- Day 5: Sunny with a Chance of Asteroids ---

You're starting to sweat as the ship makes its way toward Mercury. The Elves
suggest that you get the air conditioner working by upgrading your ship computer
to support the Thermal Environment Supervision Terminal.

The Thermal Environment Supervision Terminal (TEST) starts by running a
diagnostic program (your puzzle input). The TEST diagnostic program will run on
your existing Intcode computer after a few modifications:

First, you'll need to add two new instructions:

Opcode 3 takes a single integer as input and saves it to the position given by
its only parameter. For example, the instruction 3,50 would take an input value
and store it at address 50.

Opcode 4 outputs the value of its only parameter. For example, the instruction
4,50 would output the value at address 50.

Programs that use these instructions will come with documentation that explains
what should be connected to the input and output. The program 3,0,4,0,99 outputs
whatever it gets as input, then halts.

Second, you'll need to add support for parameter modes:

Each parameter of an instruction is handled based on its parameter mode. Right
now, your ship computer already understands parameter mode 0, position mode,
which causes the parameter to be interpreted as a position - if the parameter is
50, its value is the value stored at address 50 in memory. Until now, all
parameters have been in position mode.

Now, your ship computer will also need to handle parameters in mode 1, immediate
mode. In immediate mode, a parameter is interpreted as a value - if the
parameter is 50, its value is simply 50.

Parameter modes are stored in the same value as the instruction's opcode. The
opcode is a two-digit number based only on the ones and tens digit of the value,
that is, the opcode is the rightmost two digits of the first value in an
instruction. Parameter modes are single digits, one per parameter, read
right-to-left from the opcode: the first parameter's mode is in the hundreds
digit, the second parameter's mode is in the thousands digit, the third
parameter's mode is in the ten-thousands digit, and so on. Any missing modes are
0.

For example, consider the program 1002,4,3,4,33.

The first instruction, 1002,4,3,4, is a multiply instruction - the rightmost two
digits of the first value, 02, indicate opcode 2, multiplication. Then, going
right to left, the parameter modes are 0 (hundreds digit), 1 (thousands digit),
and 0 (ten-thousands digit, not present and therefore zero):

ABCDE
 1002

DE - two-digit opcode,      02 == opcode 2
 C - mode of 1st parameter,  0 == position mode
 B - mode of 2nd parameter,  1 == immediate mode
 A - mode of 3rd parameter,  0 == position mode,
                                  omitted due to being a leading zero

This instruction multiplies its first two parameters. The first parameter, 4 in
position mode, works like it did before - its value is the value stored at
address 4 (33). The second parameter, 3 in immediate mode, simply has value 3.
The result of this operation, 33 * 3 = 99, is written according to the third
parameter, 4 in position mode, which also works like it did before - 99 is
written to address 4.

Parameters that an instruction writes to will never be in immediate mode.

Finally, some notes:

It is important to remember that the instruction pointer should increase by the
number of values in the instruction after the instruction finishes. Because of
the new instructions, this amount is no longer always 4.

Integers can be negative: 1101,100,-1,4,0 is a valid program (find 100 + -1,
store the result in position 4).

The TEST diagnostic program will start by requesting from the user the ID of the
system to test by running an input instruction - provide it 1, the ID for the
ship's air conditioner unit.

It will then perform a series of diagnostic tests confirming that various parts
of the Intcode computer, like parameter modes, function correctly. For each
test, it will run an output instruction indicating how far the result of the
test was from the expected value, where 0 means the test was successful.
Non-zero outputs mean that a function is not working correctly; check the
instructions that were run before the output instruction to see which one
failed.

Finally, the program will output a diagnostic code and immediately halt. This
final output isn't an error; an output followed immediately by a halt means the
program finished. If all outputs were zero except the diagnostic code, the
diagnostic program ran successfully.

After providing 1 to the only input instruction and passing all the tests, what
diagnostic code does the program produce?   
*)

let input =
  Advent.read_lines "day02" () ~for_tests:["1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,9,19,23,2,23,13,27,1,27,9,31,2,31,6,35,1,5,35,39,1,10,39,43,2,43,6,47,1,10,47,51,2,6,51,55,1,5,55,59,1,59,9,63,1,13,63,67,2,6,67,71,1,5,71,75,2,6,75,79,2,79,6,83,1,13,83,87,1,9,87,91,1,9,91,95,1,5,95,99,1,5,99,103,2,13,103,107,1,6,107,111,1,9,111,115,2,6,115,119,1,13,119,123,1,123,6,127,1,127,5,131,2,10,131,135,2,135,10,139,1,13,139,143,1,10,143,147,1,2,147,151,1,6,151,0,99,2,14,0,0"]
  |> List.hd_exn
  |> String.split ~on:',' 
  |> List.map ~f:int_of_string
;;

module T : sig
  include Day.T
end = struct
  let name = "--- Day 5: Sunny with a Chance of Asteroids ---"

  let op1 pos input =
    let first = Array.get input (Array.get input (pos + 1)) in
    let second = Array.get input (Array.get input (pos + 2)) in
    let destination = Array.get input (pos + 3) in
    Array.set input destination (first + second)
  ;;

  let op2 pos input =
    let first = Array.get input (Array.get input(pos + 1)) in
    let second = Array.get input (Array.get input(pos + 2)) in
    let destination = Array.get input (pos + 3) in
    Array.set input destination (first * second)
  ;;

  let make_solver input = let rec solve pos =
    let op1 pos = op1 pos input in
    let op2 pos = op2 pos input in
    let opcode = Array.get input pos in
    let pos =
      match opcode with
        | 1 -> op1 pos; pos + 4;
        | 2 -> op2 pos; pos + 4;
        | 99 -> -1;
        | _ -> printf "ERROR: %i\n" pos; -1;
    in
    match pos with
      | -1 -> ();
      | _ -> solve pos;
    in solve
  ;;

  let check_candidate noun verb =
    let input = Array.of_list input in
    let solve = make_solver input in
    let restore_1202_state () =
      Array.set input 1 noun;
      Array.set input 2 verb;
    in
    restore_1202_state ();
    solve 0;
    Array.get input 0
  ;;

  let part1 = check_candidate 12 2

  (* "Good, the new computer seems to be working correctly! Keep it nearby
  during this mission - you'll probably use it again. Real Intcode computers
  support many more features than your new one, but we'll let you know what they
  are as you need them."

  "However, your current priority should be to complete your gravity assist
  around the Moon. For this mission to succeed, we should settle on some
  terminology for the parts you've already built."

  Intcode programs are given as a list of integers; these values are used as the
  initial state for the computer's memory. When you run an Intcode program, make
  sure to start by initializing memory to the program's values. A position in
  memory is called an address (for example, the first value in memory is at
  "address 0").

  Opcodes (like 1, 2, or 99) mark the beginning of an instruction. The values
  used immediately after an opcode, if any, are called the instruction's
  parameters. For example, in the instruction 1,2,3,4, 1 is the opcode; 2, 3,
  and 4 are the parameters. The instruction 99 contains only an opcode and has
  no parameters.

  The address of the current instruction is called the instruction pointer; it
  starts at 0. After an instruction finishes, the instruction pointer increases
  by the number of values in the instruction; until you add more instructions to
  the computer, this is always 4 (1 opcode + 3 parameters) for the add and
  multiply instructions. (The halt instruction would increase the instruction
  pointer by 1, but it halts the program instead.)

  "With terminology out of the way, we're ready to proceed. To complete the
  gravity assist, you need to determine what pair of inputs produces the output
  19690720."

  The inputs should still be provided to the program by replacing the values at
  addresses 1 and 2, just like before. In this program, the value placed in
  address 1 is called the noun, and the value placed in address 2 is called the
  verb. Each of the two input values will be between 0 and 99, inclusive.

  Once the program has halted, its output is available at address 0, also just
  like before. Each time you try a pair of inputs, make sure you first reset the
  computer's memory to the values in the program (your puzzle input) - in other
  words, don't reuse memory from a previous attempt.

  Find the input noun and verb that cause the program to produce the output
  19690720. What is 100 * noun + verb? (For example, if noun=12 and verb=2, the
  answer would be 1202.) *)

  let rec try_all noun verb =
    let x = check_candidate noun verb in
    let try_next () =
      match try_all (noun + 1) verb with
        | Some x -> Some x
        | None -> try_all noun (verb + 1)
    in
    match x = 19690720 with
      | true -> Some (100 * noun + verb)
      | false -> match x > 19690720 with
          | true -> None
          | false -> try_next ()
  ;;

  let part2 = Option.value_exn (try_all 12 2)

end
