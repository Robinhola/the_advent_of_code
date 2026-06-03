#include <algorithm>
#include <iostream>

#include "day08.h"
#include "day09.h"
#include "day10.h"

bool isNumber(const std::string &in) {
    return std::all_of(in.cbegin(), in.cend(), isdigit);
}

std::string usage = "first argument is the day number, second is argument is "
                    "either a filename or stdin";

int main(int argc, char *argv[]) {
    std::cout << "Hello, World!" << std::endl;

    if (argc < 3) {
        std::cerr << usage << std::endl;
        throw std::invalid_argument("Missing arguments");
    }

    std::string problemNumberArg = std::string(argv[1]);
    if (!isNumber(problemNumberArg)) {
        throw std::invalid_argument(
            "First argument should be the problem number");
    }

    std::string inputArg = std::string(argv[2]);

    int problemNumber = std::stoi(problemNumberArg);
    switch (problemNumber) {
    case 8:
        std::cout << "Solving Day08" << std::endl;
        std::cout << Day08::solve() << std::endl;
        break;
    case 9:
        std::cout << "Solving Day09 with " << inputArg << std::endl;
        if ("stdin" == inputArg) {
            std::cout << Day09::solve() << std::endl;
        } else {
            std::cout << Day09::solve(inputArg) << std::endl;
        }
        break;
    case 10:
        std::cout << "Solving Day10 with " << inputArg << std::endl;
        if ("stdin" == inputArg) {
            std::cout << Day10::solve() << std::endl;
        } else {
            std::cout << Day10::solve(inputArg) << std::endl;
        }
        break;
    default:
        throw std::invalid_argument(
            "First argument should be the problem number");
        break;
    }

    return 0;
}
