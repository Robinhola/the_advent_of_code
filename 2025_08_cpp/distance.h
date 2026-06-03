#pragma once
#include "node.h"

struct Distance {
    long long val;
    int left;
    int right;

    Distance(const std::vector<Node> &nodes, int left, int right)
        : val(distance(nodes[left], nodes[right])), left(left), right(right) {}

    auto operator<=>(const Distance &b) const { return val <=> b.val; }
    bool operator==(const Distance &b) const { return val == b.val; }
};
