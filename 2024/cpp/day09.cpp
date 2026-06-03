#include "day09.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <vector>

namespace Day09_util {
using Row  = std::vector<int>;
using Rows = std::vector<Row>;

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

struct Input {
    Rows rows;

    Input(const Rows &rows) : rows(rows) {}

    static Input from_sstream(std::istream &stream) {
        Rows        rows;
        std::string line;

        while (std::getline(stream, line)) {
            Row                row;
            std::istringstream iss(line);
            int                number;

            while (iss >> number) {
                row.push_back(number);
            }

            rows.push_back(row);
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
    for (const auto &row : input.rows) {
        int n           = row.size();
        Row transformed = Row(row);
        for (int i = 1; i <= n - 2; i++) {
            for (int j = 0; j < n - i; j++) {
                transformed[j] = transformed[j + 1] - transformed[j];
            }
        }

        result << "Row before: " << transformed << std::endl;

        for (int i = 1; i < n; i++) {
            transformed[i] = transformed[i - 1] + transformed[i];
        }

        result << "Row after: " << transformed << std::endl;

        total += transformed[n - 1];
    }

    result << "Part1: " << total;
    return result.str();
}

std::string part2(const Input &input) {
    std::ostringstream result;
    int                total = 0;
    for (const auto &row : input.rows) {
        int n           = row.size();
        Row transformed = Row(row);

        for (int i = 1; i <= n - 2; i++) {
            for (int j = n - 1; j >= 0 + i; j--) {
                transformed[j] = transformed[j] - transformed[j - 1];
            }
        }

        result << "Row before: " << transformed << std::endl;

        for (int i = n - 2; i >= 0; i--) {
            transformed[i] = transformed[i] - transformed[i + 1];
        }

        result << "Row after: " << transformed << std::endl;

        total += transformed[0];
    }

    result << "Part2: " << total;
    return result.str();
}

std::string _solve(const Input &input) {
    std::ostringstream result;
    result << part1(input) << "\n" << part2(input) << std::endl;
    return result.str();
}

} // namespace Day09_util

using namespace Day09_util;

std::string Day09::solve() {
    Input input = Input::from_stdin();
    return _solve(input);
}

std::string Day09::solve(const std::string &filename) {
    Input input = Input::from_file(filename);
    return _solve(input);
}
