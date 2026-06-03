#pragma once
#include <fstream>
#include <memory>
#include <sstream>
#include <string>

const std::string sample = R"(162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689)";

struct Args {
    std::unique_ptr<std::istream> input;
    int limit;

    static Args parse(int argc, char *argv[]) {
        if (argc > 1)
            return {std::make_unique<std::ifstream>(argv[1]), 1000};
        return {std::make_unique<std::istringstream>(sample), 10};
    }
};
