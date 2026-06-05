#include "args.h"
#include <algorithm>
#include <cassert>
#include <climits>
#include <iostream>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>

struct Machine {
    int lightDiagram;
    std::vector<std::vector<int>> wiring;
    std::vector<int> joltageRequirements;
};

std::istream &operator>>(std::istream &is, Machine &machine) {
    std::string line;
    if (!std::getline(is, line))
        return is;

    std::istringstream ss(line);
    machine.lightDiagram        = parseDiagram(ss);
    machine.wiring              = parseWiring(ss);
    machine.joltageRequirements = parseJoltageRequirements(ss);

    return is;
}

std::ostream &operator<<(std::ostream &os, const std::vector<int> &v) {
    for (auto x : v) {
        os << x << ';';
    }
    return os;
}

constexpr int MAXSIZE = 10;
std::string lightDiagram(int val) {
    return std::bitset<MAXSIZE>(val).to_string();
}
std::string printLightDiagram(const Machine &machine) {
    return lightDiagram(machine.lightDiagram)
        .substr(MAXSIZE - machine.joltageRequirements.size());
}

std::ostream &operator<<(std::ostream &os, const Machine &machine) {
    os << printLightDiagram(machine) << "\t|";
    for (auto v : machine.wiring) {
        os << v << " ";
    }
    os << "==\t" << machine.joltageRequirements;

    return os;
}

int pressButton(const std::vector<int> &button, int current) {
    for (auto v : button) {
        current ^= (1 << v);
    }
    return current;
}

int bfs(const Machine &machine) {
    int turns = 0;
    int goal  = machine.lightDiagram;

    std::queue<int> q;
    q.push(0);
    std::unordered_set<int> seen;
    while (!q.empty()) {
        turns++;
        int turnSize = q.size();
        while (turnSize--) {
            int current = q.front();
            q.pop();
            for (auto &b : machine.wiring) {
                int val = pressButton(b, current);
                if (val == goal) {
                    return turns;
                }
                if (!seen.count(val)) {
                    seen.insert(val);
                    q.push(val);
                }
            }
        }
    }
    return -1;
}

struct VectorHash {
    size_t operator()(const std::vector<int> &v) const {
        size_t seed = v.size();
        for (auto x : v) {
            seed ^= x + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
        return seed;
    }
};

using SET = std::unordered_set<std::vector<int>, VectorHash>;

std::pair<bool, std::vector<int>> pressButton2(const std::vector<int> &button,
                                               std::vector<int> current) {
    for (auto v : button) {
        if (current[v]-- < 0) {
            return {false, current};
        }
    }
    return {true, current};
}

int bfs2(const Machine &machine) {
    int turns             = 0;
    std::vector<int> goal = std::vector(machine.joltageRequirements.size(), 0);

    std::queue<std::vector<int>> q;
    q.push(machine.joltageRequirements);

    SET seen;
    while (!q.empty()) {
        turns++;
        int turnSize = q.size();
        std::cout << turnSize << " " << std::flush;
        while (turnSize--) {
            std::vector<int> current = q.front();
            q.pop();
            for (auto &b : machine.wiring) {
                auto [valid, val] = pressButton2(b, current);
                if (!valid) {
                    continue;
                }
                if (val == goal) {
                    return turns;
                }
                if (!seen.count(val)) {
                    seen.insert(val);
                    q.push(val);
                }
            }
        }
    }
    return -1;
}

// (0,6,8) (1,2,4,5,6,9) (2,5,7,8) (1,3) (0,1,3,4,5,6,7,9) (2,5) (4,5,7,8,9)
// (1,2,4,5,6,7,8) {24,40,33,27,205,225,37,220,224,192} min => 0 24; 3 27; 2 33;
// etc 24 27

using Ints = std::vector<int>;
void solve(const Machine &m) {
    std::unordered_map<int, int> maxPerLight;
    for (int i = 0; i < m.joltageRequirements.size(); ++i) {
        maxPerLight[i] = m.joltageRequirements[i];
    }
    std::unordered_map<int, int> maxPerButton;
    for (int i = 0; i < m.wiring.size(); ++i) {
        maxPerButton[i] = INT_MAX;
        for (auto v : m.wiring[i]) {
            maxPerButton[i] = std::min(maxPerButton[i], maxPerLight[v]);
        }
    }
    std::cout << m << std::endl;
    for (auto [k, v] : maxPerButton) {
        std::cout << k << ":" << v << " ";
    }
    std::cout << std::endl;
}

struct Parts {
    int part1(const std::vector<Machine> &machines) {
        int total = 0;
        for (auto &m : machines) {
            int value = bfs(m);
            total += value;
        }
        return total;
    }
    int part2(std::vector<Machine> &machines) {
        int total = 0;
        for (auto &m : machines) {
            solve(m);
        }
        return total;

        return 0;
    }
};

int main(int argc, char *argv[]) {
    auto args = Args::parse(argc, argv);

    std::vector<Machine> nodes;
    for (auto &m : std::ranges::istream_view<Machine>(*args.input)) {
        nodes.push_back(std::move(m));
    }

    Parts p{};
    std::cout << p.part1(nodes) << std::endl;
    std::cout << p.part2(nodes) << std::endl;
}
