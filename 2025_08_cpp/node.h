#pragma once
#include "coord.h"
#include <vector>

struct Node {
    int parent;
    int rank;
    int size;
    Coord coord;

    Node(Coord &&coord, int i) : parent(i), rank(0), size(1), coord(coord) {}

    operator Coord() const { return coord; }
    operator int() const { return size; }
};

inline int find(std::vector<Node> &nodes, int a) {
    if (nodes[a].parent != a)
        nodes[a].parent = find(nodes, nodes[a].parent);
    return nodes[a].parent;
}

inline bool unite(std::vector<Node> &nodes, int a, int b) {
    a = find(nodes, a);
    b = find(nodes, b);
    if (a == b)
        return false;
    if (nodes[a].rank < nodes[b].rank)
        std::swap(a, b);
    if (nodes[a].rank == nodes[b].rank)
        nodes[a].rank++;
    nodes[a].size += nodes[b].size;
    nodes[b].parent = a;
    return nodes[a].size == (int)nodes.size();
}
