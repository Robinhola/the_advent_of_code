#include <array>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <ranges>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

const std::string sample = R"([.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5})";

struct Args {
    std::unique_ptr<std::istream> input;

    static Args parse(int argc, char *argv[]) {
        if (argc > 1) {
            return {std::make_unique<std::ifstream>(argv[1])};
        }
        return {std::make_unique<std::istringstream>(sample)};
    }
};

struct Line {};

std::istream &operator>>(std::istream &is, Line &line) { return is; }

std::ostream &operator<<(std::ostream &os, const Line &line) { return os; }

struct Parts {
    int part1() { return 0; }
    int part2() { return 0; }
};

int main(int argc, char *argv[]) {
    auto args = Args::parse(argc, argv);

    Parts p{};
    std::cout << p.part1() << std::endl;
    std::cout << p.part2() << std::endl;
}
