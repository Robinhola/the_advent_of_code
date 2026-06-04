#include <cassert>
#include <iostream>

#include "args.h"

struct Parts {
    int part1() { return 0; }
    int part2() { return 0; }
};

int main(int argc, char *argv[]) {
    auto args   = Args::parse(argc, argv);
    Input input = parse(*args.input);

    int count = 0, size;
    for (auto &r : input.regions) {
        size = r.length * r.width;
        for (auto c : r.counts)
            size -= c * 9;
        count += size >= 0;
    }

    Parts p{};
    std::cout << count << std::endl;
    std::cout << p.part2() << std::endl;
}
