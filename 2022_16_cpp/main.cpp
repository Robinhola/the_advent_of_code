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

const std::string sample =
    R"(Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II)";

struct Args {
    std::unique_ptr<std::istream> input;

    static Args parse(int argc, char *argv[]) {
        if (argc > 1) {
            return {std::make_unique<std::ifstream>(argv[1])};
        }
        return {std::make_unique<std::istringstream>(sample)};
    }
};

struct Valve {
    std::string name;
    int flowRate;
    std::vector<std::string> leads;
};

std::istream &operator>>(std::istream &is, Valve &valve) {
    // "Valve HH has flow rate=22; tunnel leads to valve GG"
    std::string wholeLine;
    if (!std::getline(is, wholeLine))
        return is;

    std::istringstream ss(wholeLine);
    std::string word;

    ss >> word >> valve.name;

    std::getline(ss, word, '=');
    ss >> valve.flowRate;

    while (ss >> word && word != "valve" && word != "valves") {
    }
    while (ss >> word) {
        if (word.back() == ',')
            word.pop_back();
        valve.leads.push_back(std::move(word));
    }

    return is;
}

std::ostream &operator<<(std::ostream &os, const Valve &valve) {
    os << valve.name << ":" << valve.flowRate << "\t=>\t";
    for (auto &v : valve.leads) {
        os << v << "|";
    }
    return os;
}

struct Parts {
    int part1() { return 0; }
    int part2() { return 0; }
};

int main(int argc, char *argv[]) {
    auto args = Args::parse(argc, argv);

    std::vector<Valve> valves;
    for (auto &&v : std::ranges::istream_view<Valve>(*args.input)) {
        std::cout << v << std::endl;
        valves.push_back(std::move(v));
    }

    Parts p{};
    std::cout << p.part1() << std::endl;
    std::cout << p.part2() << std::endl;
}
