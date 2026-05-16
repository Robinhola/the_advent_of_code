import collections
import math
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


def distance_of(a: dict, b: dict) -> int:
    return math.sqrt(sum((b[x] - a[x]) ** 2 for x in ("x", "y", "z")))


def per_distances(nodes: typing.List[dict]) -> typing.List[dict]:
    distances = []
    for i, a in enumerate(nodes):
        for j, b in enumerate(nodes[i + 1 :]):
            d = distance_of(a, b)
            distances.append({"distance": d, "a": i, "b": j + i + 1})
    distances.sort(key=lambda x: x["distance"])
    return distances


def connect(circuits_to_nodes, nodes_to_circuits, a, b):
    a_in = a in nodes_to_circuits
    b_in = b in nodes_to_circuits
    if a_in and b_in:
        if nodes_to_circuits[a] == nodes_to_circuits[b]:
            return False
        circuit_a = nodes_to_circuits[a]
        circuit_b = nodes_to_circuits[b]
        circuits_to_nodes[circuit_a].update(circuits_to_nodes[circuit_b])
        for node in circuits_to_nodes[circuit_b]:
            nodes_to_circuits[node] = circuit_a
        del circuits_to_nodes[circuit_b]
    elif a_in:
        circuit = nodes_to_circuits[a]
        circuits_to_nodes[circuit].add(b)
        nodes_to_circuits[b] = circuit
    elif b_in:
        circuit = nodes_to_circuits[b]
        circuits_to_nodes[circuit].add(a)
        nodes_to_circuits[a] = circuit
    else:
        new_circuit = a
        circuits_to_nodes[new_circuit].add(a)
        circuits_to_nodes[new_circuit].add(b)
        nodes_to_circuits[a] = new_circuit
        nodes_to_circuits[b] = new_circuit
    return True


def part1(lines: typing.List[str], how_many: int) -> int:
    nodes = [parse(l) for l in lines]
    distances = per_distances(nodes)
    circuits_to_nodes = collections.defaultdict(set)
    nodes_to_circuits = {}

    for i, d in enumerate(distances[:how_many]):
        a, b = d["a"], d["b"]
        connect(circuits_to_nodes, nodes_to_circuits, a, b)

    sizes = sorted(circuits_to_nodes.values(), key=len, reverse=True)
    return len(sizes[0]) * len(sizes[1]) * len(sizes[2])


def part2(lines: typing.List[str]):
    nodes = [parse(l) for l in lines]
    distances = per_distances(nodes)
    circuits_to_nodes = collections.defaultdict(set)
    nodes_to_circuits = {}

    for i, d in enumerate(distances):
        a, b = d["a"], d["b"]
        connect(circuits_to_nodes, nodes_to_circuits, a, b)

        if len(nodes_to_circuits) == len(nodes) and len(circuits_to_nodes) == 1:
            break

    return nodes[a]["x"] * nodes[b]["x"]


def main():
    lines = tuple(l.strip() for l in sys.stdin)
    p1 = part1(lines, 1000)
    print(f"Part1: {p1}")
    p2 = part2(lines)
    print(f"Part2: {p2}")


def test_distances() -> None:
    assert (
        distance_of({"x": 0, "y": 0, "z": 0}, {"x": 1, "y": 1, "z": 1})
        == 1.7320508075688772
    )
    assert distance_of({"x": 0, "y": 0, "z": 0}, {"x": 0, "y": 0, "z": 1}) == 1
    assert distance_of({"x": 0, "y": 0, "z": 0}, {"x": 0, "y": 0, "z": 0}) == 0


def test_closest() -> None:
    nodes = [parse(l) for l in sample.splitlines()]
    distances = per_distances(nodes)
    d, ai, bi = distances[0]["distance"], distances[0]["a"], distances[0]["b"]
    a = nodes[ai]
    b = nodes[bi]
    assert a == {"x": 162, "y": 817, "z": 812}
    assert b == {"x": 425, "y": 690, "z": 689}
    d, ai, bi = distances[1]["distance"], distances[1]["a"], distances[1]["b"]
    a = nodes[ai]
    b = nodes[bi]
    assert a == {"x": 162, "y": 817, "z": 812}
    assert b == {"x": 431, "y": 825, "z": 988}
    d, ai, bi = distances[2]["distance"], distances[2]["a"], distances[2]["b"]
    a = nodes[ai]
    b = nodes[bi]
    assert a == {"x": 906, "y": 360, "z": 560}
    assert b == {"x": 805, "y": 96, "z": 715}


def test_solution() -> None:
    r = part1(sample.splitlines(), 10)
    assert r == 40
    r = part2(sample.splitlines())
    assert r == 25272


if __name__ == "__main__":
    main()
