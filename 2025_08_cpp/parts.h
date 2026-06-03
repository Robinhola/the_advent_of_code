#pragma once
#include "distance.h"
#include "node.h"
#include <functional>
#include <numeric>
#include <set>

struct Parts {
    std::vector<Node> &nodes;
    std::vector<Distance> &distances;
    int limit;

    long long part1() {
        for (int i = 0; i < limit; ++i)
            unite(nodes, distances[i].left, distances[i].right);

        std::set<int> sizes;
        for (auto &n : nodes) {
            if (sizes.size() < 3 || n.size >= *sizes.begin())
                sizes.insert(n);
            if (sizes.size() > 3)
                sizes.erase(sizes.begin());
        }

        return std::reduce(sizes.begin(), sizes.end(), 1, std::multiplies<>());
    }

    long long part2() {
        int i = limit;
        while (!unite(nodes, distances[i].left, distances[i].right))
            i++;
        auto &[_, l, r] = distances[i];
        return nodes[l].coord.x * nodes[r].coord.x;
    }
};
