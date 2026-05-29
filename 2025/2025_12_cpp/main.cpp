#include <array>
#include <fstream>
#include <iostream>
#include <memory>
#include <ranges>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

const std::string sample = R"()";

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

std::istream &operator>>(std::istream &is, Line &line) {
    // parses header: w1 w2 w3
    // std::string rest;
    // std::getline(is, line.header, ':');
    // std::getline(is, rest);
    // std::istringstream ss(rest);
    // line.values = {std::istream_iterator<std::string>(ss), {}};
    return is;
}

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
