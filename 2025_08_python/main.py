import sys
import typing
from pprint import pprint

sample = """162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"""


def parse(a: str):
    x, y, z = a.split(",")
    return {"x": int(x), "y": int(y), "z": int(z)}


def distance(a: str, b: str):
    a = parse(a)
    b = parse(b)
    return sum(
        (
            (b["x"] - a["x"]) ** 2,
            (b["y"] - a["y"]) ** 2,
            (b["z"] - a["z"]) ** 2,
        )
    )


def all_combinations(l):
    combs = []
    for i, a in enumerate(l[:-1]):
        for b in l[i + 1 :]:
            combs.append((a, b, distance(a, b)))
    combs.sort(key=lambda x: x[2])
    return combs


def connect(circuits, nodes_to_group, connections):
    for a, b, _ in connections:
        pprint([a, b])
        if a in nodes_to_group and b in nodes_to_group:
            groupa = nodes_to_group[a]
            groupb = nodes_to_group[b]
            if groupa == groupb:
                continue
            circuits[groupa] = circuits[groupa] | circuits[groupb]
            circuits[groupb] = set()
            nodes_to_group[b] = groupa
        elif a in nodes_to_group:
            groupa = nodes_to_group[a]
            circuits[groupa].add(b)
            nodes_to_group[b] = groupa
        elif b in nodes_to_group:
            groupb = nodes_to_group[b]
            circuits[groupb].add(a)
            nodes_to_group[a] = groupb
        else:
            group = len(circuits)
            circuits[group] = {a, b}
            nodes_to_group[a] = group
            nodes_to_group[b] = group


def part1(lines: typing.List[str], debug=False):
    limit = 10 if debug else 1000
    print("Limit", limit)
    top_connections = all_combinations(lines)[:limit]
    circuits = dict()
    nodes_to_group = dict()
    connect(circuits, nodes_to_group, top_connections)
    if debug:
        pprint(circuits)
    top = sorted(len(c) for c in circuits.values())
    pprint(top[-3:])
    a, b, c = top[-3:]
    return a * b * c


def part2(lines: typing.List[str]):
    return 0


def main():
    print(f"{part1(sample.splitlines(), debug=True)}")
    lines = tuple(l.strip() for l in sys.stdin)
    # p1 = part1(lines)
    p1 = 0
    p2 = part2(lines)
    print(f"Part1: {p1}")
    print(f"Part2: {p2}")


if __name__ == "__main__":
    main()
