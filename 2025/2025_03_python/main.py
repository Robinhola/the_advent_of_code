import sys
import typing


def best_of(l: list, i: int, n: int, number: str):
    if n == 0:
        return int(number)
    n = n - 1
    m = max(l[i : len(l) - n])
    index = l[i:].index(m) + i
    return best_of(l, index + 1, n, number + m)


def part1(lines: typing.List[str]):
    return sum(best_of(l, 0, 2, "") for l in lines)


def part2(lines: typing.List[str]):
    return sum(best_of(l, 0, 12, "") for l in lines)


def main():
    lines = tuple(l.strip() for l in sys.stdin)
    p1 = part1(lines)
    p2 = part2(lines)
    print(f"Part1: {p1}")
    print(f"Part2: {p2}")


if __name__ == "__main__":
    main()
