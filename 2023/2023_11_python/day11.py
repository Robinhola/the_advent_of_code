#!/usr/bin/python3

from dataclasses import dataclass
from pprint import pprint
import argparse

@dataclass(eq=True, frozen=True)
class Coordinate:
    x: int
    y: int

    @staticmethod
    def add(left, right):
        x = left.x + right.x
        y = left.y + right.y
        return Coordinate(x=x, y=y)

    @staticmethod
    def mult(left, right):
        x = left.x * right.x
        y = left.y * right.y
        return Coordinate(x=x, y=y)

def make_debug(args):
    if args.debug:
        def debug(*args, **kwargs):
            print(*args, **kwargs)

        def if_debug(f, *args, **kwargs):
            f(*args, **kwargs)

    else:
        def debug(*args, **kwargs):
            pass
        def if_debug(f, *args, **kwargs):
            pass

    return debug, if_debug

def from_lines(lines):
    return [ l.strip() for l in lines ]

def print_distances(matrix):
    for row in matrix:
        print("".join([str(x) for x in row]))

C = Coordinate

def compute_actual_postions(matrix, expanding_rows, expanding_columns, offset):
    x_counter = 0
    y_counter = 0
    actual_positions = []
    for y, row in enumerate(matrix):
        if y in expanding_rows:
            y_counter += offset 
            continue

        x_counter = 0
        for x, val in enumerate(row):
            if x in expanding_columns:
                x_counter += offset
                continue

            if row[x] == '#':
                actual_positions.append(
                    C(x=x + x_counter, y=y + y_counter)
                )

    return actual_positions


def compute_total_distances(actual_positions):
    minimum_distance_total = 0
    queue = list(actual_positions)
    while queue:
        first = queue.pop()
        for second in queue:
            distance = abs(first.x - second.x) + abs(first.y - second.y)
            minimum_distance_total += distance
    return minimum_distance_total

def main(args):
    debug, if_debug = make_debug(args)
    debug(args)

    matrix = []
    with open(args.filename) as f:
        lines = f.readlines()
        matrix = from_lines(lines)

    starting_positions = [
        C(x=x, y=y)
        for y, row in enumerate(matrix)
        for x, val in enumerate(row)
        if val != '.'
    ] 

    expanding_rows = [
        y
        for y in range(len(matrix))
        if y not in {
            g.y
            for g in starting_positions
        }
    ]

    expanding_columns = [
        x
        for x in range(len(matrix[0]))
        if x not in {
            g.x
            for g in starting_positions
        }
    ]

    actual_positions = compute_actual_postions(matrix, expanding_rows, expanding_columns, offset=1)

    if args.debug:
        expanded_matrix = [
            ['.'] * (len(matrix[0]) + len(expanding_columns))
            for _ in range(len(matrix) + len(expanding_rows)) 
        ]
        for p in actual_positions:
            expanded_matrix[p.y][p.x] = '#'
    else:
        expanded_matrix = []

    minimum_distance_total = compute_total_distances(actual_positions)

    if_debug(print_distances, matrix)
    if_debug(print_distances, expanded_matrix)
    debug(starting_positions)
    debug(expanding_columns)
    debug(expanding_rows)
    print(minimum_distance_total)

    for offset in (10, 100, 1_000_000):
        actual_positions = compute_actual_postions(matrix, expanding_rows, expanding_columns, offset=offset - 1)
        minimum_distance_total = compute_total_distances(actual_positions)
        print(minimum_distance_total)


def tests():
    pass

def args():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--filename")
    parser.add_argument("-d", "--debug", action="store_true")
    parser.add_argument("-t", "--run-tests", action="store_true")
    return parser.parse_args()

if __name__ == "__main__":
    if (args := args()).run_tests:
        tests()
    else:
        main(args)
