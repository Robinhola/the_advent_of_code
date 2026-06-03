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


@dataclass
class Pipe:
    from_: Coordinate
    to_: Coordinate


C = Coordinate
P = Pipe

NORTH = Coordinate(x=0, y=-1)
SOUTH = Coordinate(x=0, y=1)
SOUTH2 = Coordinate(x=0, y=2)
EAST = Coordinate(x=1, y=0)
EAST2 = Coordinate(x=2, y=0)
WEST = Coordinate(x=-1, y=0)

table = {
    "|": Pipe(from_=NORTH, to_=SOUTH),
    "-": Pipe(from_=EAST, to_=WEST),
    "L": Pipe(from_=NORTH, to_=EAST),
    "J": Pipe(from_=NORTH, to_=WEST),
    "7": Pipe(from_=SOUTH, to_=WEST),
    "F": Pipe(from_=SOUTH, to_=EAST),
    ".": None,
    "S": None,
}


def from_lines(lines):
    return [list(l.strip()) for l in lines]


def previous_position(current, val):
    if val is None:
        return

    pipe = table[val]
    if pipe:
        return Coordinate.add(current, pipe.from_)


def next_position(current, val):
    if val is None:
        return

    pipe = table[val]
    if pipe:
        return Coordinate.add(current, pipe.to_)


def find_start(matrix):
    for y in range(0, len(matrix)):
        for x in range(0, len(matrix[0])):
            if matrix[y][x] == "S":
                return Coordinate(x=x, y=y)


def set_(matrix, at, value):
    matrix[at.y][at.x] = value


def set2(matrix, atx2, value):
    return set_(matrix, Coordinate(x=atx2.x // 2, y=atx2.y // 2), value)


def get_(matrix, at):
    if 0 <= at.y < len(matrix) and 0 <= at.x < len(matrix[0]):
        return matrix[at.y][at.x]


def get2(matrix, atx2):
    boundsx = 2 * len(matrix[0]) - 1
    boundsy = 2 * len(matrix) - 1

    if 0 <= atx2.x < boundsx or 0 <= atx2.y < boundsy:
        return get_(matrix, Coordinate(x=atx2.x // 2, y=atx2.y // 2))
    else:
        return None


def print_distances(matrix):
    for row in matrix:
        print("".join([str(x) for x in row]))


def main(args):
    pprint(args)

    if args.debug:

        def debug(*args, **kwargs):
            print(*args, **kwargs)

    else:

        def debug(*args, **kwargs):
            pass

    matrix = None
    s_position = None

    with open(args.filename) as f:
        lines = f.readlines()
        matrix = from_lines(lines)

    s_position = find_start(matrix)
    debug("S-position", s_position)

    distances = [["." for _ in l] for l in matrix]
    set_(distances, s_position, 0)
    seen = {s_position}
    queue = [
        (s_position, Coordinate.add(s_position, d)) for d in (NORTH, EAST, WEST, SOUTH)
    ]
    debug(queue)
    iterations = 1
    candidate = 0
    while queue:
        next_queue = []
        debug("Iteration", iterations, queue)

        while queue:
            from_, next_ = queue.pop()
            if next_ in seen:
                debug("SEEN!", seen, next_)
                candidate = iterations - 1
                continue

            val = get_(matrix, next_)
            a_position = previous_position(next_, val)
            b_position = next_position(next_, val)
            debug(val)
            debug("From", from_)
            debug("Next", next_)
            debug(a_position, b_position)

            if from_ == a_position:
                debug("b", end="")
                seen.add(next_)
                next_queue.append((next_, b_position))
                debug(next_queue)
            elif from_ == b_position:
                debug("a", end="")
                seen.add(next_)
                next_queue.append((next_, a_position))
                debug(next_queue)
            else:
                debug("xxxxxxxxxxx")
                continue

            set_(distances, next_, iterations)
            debug("----------")

        iterations += 1
        queue = list(next_queue)

    if args.debug:
        print_distances(distances)

    print(candidate)

    looping_pipe = {Coordinate.mult(s, Coordinate(2, 2)) for s in seen}
    seen_so_far = set(looping_pipe)

    debug_matrix = [list(r) for r in matrix]

    i = 0

    boundsy = 2 * len(matrix) - 1
    boundsx = 2 * len(matrix[0]) - 1
    for y in range(boundsy):
        for x in range(boundsx):
            current = Coordinate(x=x, y=y)
            if current in seen_so_far:
                continue

            is_zone_valid = True
            current_zone = set()
            candidates = [
                Coordinate.add(current, d) for d in (NORTH, EAST, WEST, SOUTH)
            ]

            candidates.append(current)

            while candidates:
                c = candidates.pop()

                # if args.debug and c == Coordinate(x=0, y=0):
                #     __import__("pdb").set_trace()
                #
                if c in current_zone:
                    continue
                elif c in seen_so_far:
                    if get2(debug_matrix, c) == '0':
                        is_zone_valid = False

                
                seen_so_far.add(c)

                # skip if can squeeze in?
                if c.x % 2 == 0 and c.y % 2 == 1:
                    # something that connects to the south
                    if get2(matrix, Coordinate.add(c, NORTH)) in ["|", "7", "F"]:
                    # if get2(matrix, Coordinate.add(c, NORTH)) in ["|", "7", "F"]:
                        continue
                    # something that connects to the north
                    if get2(matrix, Coordinate.add(c, SOUTH2)) in ["|", "J", "L"]:
                        continue

                # skip if can squeeze in?
                if c.y % 2 == 0 and c.x % 2 == 1:
                    # something that connects to the east
                    if get2(matrix, Coordinate.add(c, WEST)) in ["-", "L", "F"]:
                        continue
                    # something that connects to the west
                    if get2(matrix, Coordinate.add(c, EAST2)) in ["-", "J", "7"]:
                        continue

                # if it's a regular pipe 
                if c.y % 2 == 0 and c.x % 2 == 0 and get2(matrix, c) != ".":
                    continue

                # if out of bounds
                if get2(matrix, c) is None:
                    is_zone_valid = False
                    continue

                current_zone.add(c)
                candidates.extend(
                    Coordinate.add(c, d) for d in (NORTH, EAST, WEST, SOUTH)
                )

            zone = [c for c in current_zone if c.y % 2 == 0 and c.x % 2 == 0]
            if is_zone_valid:
                for c in zone:
                    set2(debug_matrix, c, "I")

                n = len(zone)
                i += n
            else:
                for c in zone:
                    set2(debug_matrix, c, "0")

    if args.debug:
        print_distances(debug_matrix)

    print(i)


def tests():
    for current, val in (
        (Coordinate(0, 0), "J"),
        (Coordinate(0, 0), "-"),
        (Coordinate(0, 0), "."),
    ):
        print("Current", current, "Val", val)
        print("Previous", previous_position(current, val))
        print("Next", next_position(current, val))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--filename")
    parser.add_argument("-d", "--debug", action="store_true")
    parser.add_argument("-t", "--run-tests", action="store_true")
    args = parser.parse_args()
    if args.run_tests:
        tests()
    else:
        main(args)
