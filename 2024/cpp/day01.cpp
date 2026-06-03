#include "day01.h"

#include <iostream>
#include <stdexcept>

int first_number(const std::string &line) {
    for (const char c : line) {
        if (std::isdigit(c)) {
            return int(c) - int('0');
        }
    }
    throw std::invalid_argument("Did not find number in line: " + line);
}

std::string Day01::solve() {
    std::cout << "Hi Day1" << std::endl;
    for (std::string line; std::getline(std::cin, line);) {
        std::cout << line << std::endl;

        std::cout << first_number(line) << first_number(line) << std::endl;
    }
    return "Hi! Day1";
}
