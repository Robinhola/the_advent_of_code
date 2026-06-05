#ifndef ARGS_H
#define ARGS_H

#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <ranges>
#include <sstream>
#include <string>


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

inline int parseDiagram(std::istringstream &iss) {
    std::string token;
    std::getline(iss, token, ']'); // reads "[.##."
    token   = token.substr(1);     // strip leading '['
    int val = 0;
    for (auto it = token.rbegin(); it < token.rend(); ++it) {
        val += (*it) == '#' ? 1 : 0;
        val = val << 1;
    }
    return val >> 1;
}

inline std::vector<std::vector<int>> parseWiring(std::istringstream &iss) {
    char c;
    iss >> c;

    std::vector<std::vector<int>> wiring;

    while (c == '(') {
        std::string token;
        std::vector<int> group;

        std::getline(iss, token, ')'); // reads "3" or "1,3"

        std::istringstream groupss(token);
        std::string num;
        while (std::getline(groupss, num, ','))
            group.push_back(std::stoi(num));

        wiring.push_back(std::move(group));

        iss >> c; // consume space or '{'
    }

    return wiring;
}

inline std::vector<int> parseJoltageRequirements(std::istringstream &iss) {
    std::vector<int> joltageRequirements;

    std::string token;
    std::getline(iss, token, '}'); // reads "3,5,4,7"
    std::istringstream jss(token);
    std::string num;
    while (std::getline(jss, num, ','))
        joltageRequirements.push_back(std::stoi(num));

    return joltageRequirements;
}

#endif
