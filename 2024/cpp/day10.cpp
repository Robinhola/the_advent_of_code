#include "day10.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <vector>

namespace Day10_util {
using Value = char;
using Row   = std::string;
using Rows  = std::vector<Row>;

std::ostream &operator<<(std::ostream &os, const Row &row) {
    for (const auto &number : row) {
        os << number << "; ";
    }
    return os;
}

std::ostream &operator<<(std::ostream &os, const Rows &rows) {
    for (const auto &row : rows) {
        os << row << "\n";
    }
    return os;
}

struct Coord {
    int x;
    int y;
};

struct Input {
    Rows  rows;
    Coord starting_position;

    Input(const Rows &rows) : rows(rows) {}

    Value operator[](const Coord &coord) const {
        return rows[coord.y][coord.x];
    }

    static Input from_sstream(std::istream &stream) {
        Rows        rows;
        std::string line;

        while (std::getline(stream, line)) {
            rows.emplace_back(line);
        }
        return Input(rows);
    }

    static Input from_stdin() { return from_sstream(std::cin); }

    static Input from_file(const std::string &filename) {
        std::ifstream file(filename);

        if (!file.is_open()) {
            throw std::runtime_error("Error opening file");
        }

        return from_sstream(file);
    }
};

std::string part1(const Input &input) {
    std::ostringstream result;
    int                total = 0;
    result << total;
    return result.str();
}

std::string part2(const Input &input) {
    std::ostringstream result;
    int                total = 0;
    result << total;
    return result.str();
}

std::string _solve(const Input &input) {
    std::ostringstream result;
    result << "Input: " << input.rows << "\n"
           << "At 1;2 : " << input[Coord{1, 2}] << "\n"
           << "Part1: " << part1(input) << "\n"
           << "Part2: " << part2(input) << std::endl;
    return result.str();
}
} // namespace Day10_util

using namespace Day10_util;

std::string Day10::solve() {
    Input input = Input::from_stdin();
    return _solve(input);
}

std::string Day10::solve(const std::string &filename) {
    Input input = Input::from_file(filename);
    return _solve(input);
}
