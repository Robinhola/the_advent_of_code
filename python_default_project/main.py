import sys
import typing


def part1(lines: typing.List[str]):
    for i, l in enumerate(lines):
        print(f"Line {i}: {l}")

    return 0


def part2(lines: typing.List[str]):
    return 0


def main():
    lines = tuple(l.strip() for l in sys.stdin)
    p1 = part1(lines)
    p2 = part2(lines)
    print(f"Part1: {p1}")
    print(f"Part2: {p2}")


if __name__ == "__main__":
    main()
