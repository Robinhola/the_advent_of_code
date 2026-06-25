#include <array>
#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <ostream>
#include <ranges>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

const std::string sample = R"(>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>)";

struct Args {
    std::unique_ptr<std::istream> input;

    static Args parse(int argc, char *argv[]) {
        if (argc > 1) {
            return {std::make_unique<std::ifstream>(argv[1])};
        }
        return {std::make_unique<std::istringstream>(sample)};
    }
};

struct Line {
    std::string pattern;
};

std::istream &operator>>(std::istream &is, Line &line) {
    return is >> line.pattern;
}

std::ostream &operator<<(std::ostream &os, const Line &line) {
    return os << line.pattern;
}

/*
 ####

 .#.
 ###
 .#.

 ..#
 ..#
 ###

 #
 #
 #
 #

 ##
 ##
 */

struct Parts {
    using Coord  = std::pair<int, int>;
    using Coords = std::vector<Coord>;

    std::array<std::unordered_set<int>, 7> heights{};
    std::string *vents{};
    int highest = {-1};
    int vent    = {0};
    int piece   = {0};

    char jet() { return (*vents)[vent % vents->size()]; }

    Coords nextPiece() {
        switch (piece % 5) {
        case 0:
            return {{0, 0}, {1, 0}, {2, 0}, {3, 0}};
        case 1:
            return {{1, 2}, {0, 1}, {1, 1}, {2, 1}, {1, 0}};
        case 2:
            return {{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}};
        case 3:
            return {{0, 0}, {0, 1}, {0, 2}, {0, 3}};
        case 4:
            return {{0, 0}, {1, 0}, {0, 1}, {1, 1}};
        }
        return {};
    }

    static Coord translateCoord(Coord coord, char dir) {
        if (dir == '<') {
            return {coord.first - 1, coord.second};
        }
        if (dir == '>') {
            return {coord.first + 1, coord.second};
        }
        if (dir == 'v') {
            return {coord.first, coord.second - 1};
        }
        assert(false);
    }

    bool isValidMove(Coord coord, const Coords &deltaCoords) const {
        auto [x, y] = coord;
        for (auto [dx, dy] : deltaCoords) {
            if (x + dx < 0 || 7 <= x + dx || y + dy < 0)
                return false;
            if (heights[x + dx].contains(y + dy))
                return false;
        }
        return true;
    }

    int settlePiece(Coord coord, const Coords &deltaCoords) {
        auto [x, y] = coord;
        int my      = 0;
        for (auto [dx, dy] : deltaCoords) {
            heights[x + dx].insert(y + dy);
            my = std::max(my, y + dy);
        }
        return my;
    }

    void print() {
        std::vector<std::string> lines(highest + 2, "|.......|");
        for (int x = 0; x < 7; ++x) {
            for (auto y : heights[x]) {
                lines[y][x + 1] = '#';
            }
        }
        for (auto it = lines.rbegin(); it != lines.rend(); it++)
            std::cout << *it << "\n";
        std::cout << "+-------+\n" << std::endl;
    }

    void printMove(Coord coord, const Coords &deltaCoords) {
        int my = highest + 1;
        for (auto [dx, dy] : deltaCoords) {
            auto [x, y] = coord;
            my          = std::max(my, y + dy);
        }

        std::vector<std::string> lines(my + 1, "|.......|");

        for (int x = 0; x < 7; ++x) {
            for (auto y : heights[x]) {
                lines[y][x + 1] = '#';
            }
        }
        auto [x, y] = coord;
        for (auto [dx, dy] : deltaCoords) {
            lines[y + dy][x + dx + 1] = '@';
        }
        for (auto it = lines.rbegin(); it != lines.rend(); it++)
            std::cout << *it << "\n";
        std::cout << "+-------+" << std::endl;
    }

    int part1() {
        // for each piece,
        // check as if affected by jet
        // check as if affected by gravity
        // update
        while (piece < 2022) {
            Coords deltaCoords = nextPiece();
            Coord coord        = {2, highest + 4};
            bool keepGoing     = true;
            while (keepGoing) {
                // printMove(coord, deltaCoords);
                auto newCoord = translateCoord(coord, jet());
                if (isValidMove(newCoord, deltaCoords)) {
                    // std::cout << "Moving: " << jet() << " ";
                    coord = newCoord;
                }
                newCoord = translateCoord(coord, 'v');
                if (isValidMove(newCoord, deltaCoords)) {
                    // std::cout << "Moving: " << "v";
                    coord = newCoord;
                } else {
                    keepGoing = false;
                }
                vent++;
            }
            highest = std::max(highest, settlePiece(coord, deltaCoords));
            piece++;
            // print();
        }
        return highest + 1;
    }
    int part2() { return 0; }
};

int main(int argc, char *argv[]) {
    auto args = Args::parse(argc, argv);

    Line line;
    *args.input >> line;

    std::cout << line << std::endl;

    Parts p{};
    p.vents = &line.pattern;
    std::cout << p.part1() << std::endl;
    std::cout << p.part2() << std::endl;
}
