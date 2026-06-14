#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
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
    template <typename K, typename V> using Map = std::unordered_map<K, V>;

    Map<std::string, Valve> graph                         = {};
    Map<std::string, Map<std::string, int>> distances     = {};
    Map<int, int> flowRates                               = {};
    Map<std::string, std::array<Map<int, int>, 31>> cache = {};
    std::vector<std::string> opened                       = {};
    int fullMask                                          = {0};

    explicit Parts(std::vector<Valve> &&valves) {
        graph.reserve(valves.size());
        for (auto &v : valves) {
            auto key = v.name;
            graph.emplace(key, std::move(v));
        }
        valves.clear();

        opened   = getAllOpened();
        fullMask = 0;
        for (int i = 0; i < opened.size(); ++i) {
            flowRates[1 << i] = graph[opened[i]].flowRate;
            fullMask += 1 << i;
        }
    }

    std::unordered_map<std::string, int> &bfs(std::string valve) {
        if (distances.count(valve))
            return distances[valve];

        std::unordered_set<std::string> seen{valve};
        std::queue<std::string> queue;
        queue.push(valve);

        int distance = 0;
        while (queue.size()) {
            int size = queue.size();
            while (size-- > 0) {
                std::string node = queue.front();
                queue.pop();
                distances[valve][node] = distance;
                for (auto &v : graph[node].leads) {
                    if (seen.count(v))
                        continue;
                    seen.insert(v);
                    queue.push(v);
                }
            }
            distance++;
        }

        return distances[valve];
    }

    std::vector<std::string> getAllOpened() {
        std::vector<std::string> opened;
        for (auto &[k, v] : graph) {
            if (v.flowRate)
                opened.push_back(k);
        }
        std::sort(opened.begin(), opened.end(), [&](auto a, auto b) {
            return graph[a].flowRate > graph[b].flowRate;
        });
        return opened;
    }

    int best(int timeLeft, std::string start, int mask) {
        if (timeLeft <= 0 || mask == 0)
            return 0;

        if (cache[start][timeLeft].count(mask))
            return cache[start][timeLeft][mask];

        int result = 0;
        for (int i = 0; i < opened.size(); ++i) {
            if ((mask & (1 << i)) == 0)
                continue;
            int distance = bfs(start)[opened[i]];

            timeLeft -= (distance + 1);
            mask ^= (1 << i);

            int released   = timeLeft * flowRates[1 << i];
            int fromOthers = best(timeLeft, opened[i], mask);
            result         = std::max(result, released + fromOthers);

            mask ^= (1 << i);
            timeLeft += (distance + 1);
        }

        return cache[start][timeLeft][mask] = result;
    };

    int part1() {
        // find all open and distance from here
        // try each open
        // all turned off stop
        return best(30, "AA", fullMask);
    }

    void combination (std::vector<int>& combinations, int i, int num) {
        if (i >= opened.size()) {
            combinations.push_back(num);
            return;
        }
        combination(combinations, i + 1, num + (1 << i));
        combination(combinations, i + 1, num);
    };

    int part2() {
        // 16 valves to visit
        // 8 per group
        // 01010111000
        // for each vavle
        // open it distance is distance between 0 and i
        opened       = getAllOpened();
        int fullMask = 0;
        for (int i = 0; i < opened.size(); ++i) {
            flowRates[1 << i] = graph[opened[i]].flowRate;
            fullMask += 1 << i;
        }

        std::vector<int> combinations;
        combination(combinations, 0 , 0);

        int result = 0;

        for (int i = 0; i < combinations.size() / 2; ++i) {
            auto c    = combinations[i];
            int value = best(26, "AA", c) + best(26, "AA", c ^ fullMask);
            result    = std::max(result, value);
        }

        return result;
    }
};

int main(int argc, char *argv[]) {
    auto args = Args::parse(argc, argv);

    std::vector<Valve> valves;
    for (auto &&v : std::ranges::istream_view<Valve>(*args.input)) {
        valves.push_back(std::move(v));
    }

    Parts p{std::move(valves)};

    std::cout << p.part1() << std::endl;
    std::cout << p.part2() << std::endl;
}
