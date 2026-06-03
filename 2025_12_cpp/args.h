#ifndef ARGS_H
#define ARGS_H

#include <fstream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include <cassert>

const std::string sample = R"(0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2)";

struct Args {
    std::unique_ptr<std::istream> input;

    static Args parse(int argc, char *argv[]) {
        if (argc > 1) {
            return {std::make_unique<std::ifstream>(argv[1])};
        }
        return {std::make_unique<std::istringstream>(sample)};
    }
};

using Shape = std::array<std::string, 3>;

struct Region {
    int width, length;
    std::array<int, 6> counts;
};

struct Input {
    std::array<Shape, 6> shapes;
    std::vector<Region> regions;
};

inline Input parse(std::istream &in) {
    Input result{};
    std::string line;

    while (getline(in, line)) {
        if (line.empty())
            continue;

        const auto colon = line.find(':');
        const auto cross = line.find('x');

        if (cross == std::string::npos) {
            // Shape header: "N:"
            const int id = std::stoi(line);
            assert(0 <= id && id < 6);

            for (auto &row : result.shapes[id]) {
                getline(in, row);
            }
        } else {
            // Puzzle line: "WxL: c0 c1 c2 c3 c4 c5"
            Region region{};
            region.width  = std::stoi(line);
            region.length = std::stoi(line.substr(cross + 1));
            std::istringstream iss{line.substr(colon + 1)};
            for (auto &c : region.counts)
                iss >> c;
            result.regions.push_back(region);
        }
    }

    return result;
}

#endif
