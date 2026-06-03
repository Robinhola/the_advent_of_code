#include "args.h"
#include "parts.h"
#include <iostream>
#include <ranges>

int main(int argc, char *argv[]) {
    auto args = Args::parse(argc, argv);

    std::vector<Node> nodes;
    for (auto &c : std::ranges::istream_view<Coord>(*args.input))
        nodes.emplace_back(std::move(c), nodes.size());

    std::vector<Distance> distances;
    for (int i = 0; i < (int)nodes.size() - 1; ++i)
        for (int j = i + 1; j < (int)nodes.size(); ++j)
            distances.emplace_back(nodes, i, j);
    std::ranges::sort(distances);

    Parts p{nodes, distances, args.limit};
    std::cout << p.part1() << std::endl;
    std::cout << p.part2() << std::endl;
}
