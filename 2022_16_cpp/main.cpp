#include <algorithm>
#include <array>
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
    std::unordered_map<std::string, Valve> graph;
    std::unordered_map<std::string, std::unordered_map<std::string, int>>
        distances;
    int open{0};

    explicit Parts(std::vector<Valve> &&valves) {
        for (auto &v : valves) {
            auto key = v.name;
            if (v.flowRate)
                open++;
            graph.emplace(key, std::move(v));
        }
        valves.clear();
    }

    std::unordered_map<std::string, int> &bfs(const std::string &valve) {
        if (distances.contains(valve))
            return distances[valve];

        std::unordered_set<std::string> seen{valve};
        std::queue<std::string> queue;
        queue.push(valve);

        int distance = 0;
        while (queue.size()) {
            int size = queue.size();
            while (size-- > 0) {
                auto &node = queue.front();
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

    int bt(const std::string &valve, int minutesLeft) {
        if (minutesLeft <= 0)
            return 0;

        // we always open the valve
        int flowRate = graph[valve].flowRate;
        int ifWeOpen = 0;
        if (flowRate != 0) {
            minutesLeft--;
            ifWeOpen = flowRate * minutesLeft;
        } else {
            assert(valve == "AA");
        }

        graph[valve].flowRate = 0;
        int bestFromHere      = 0;
        for (auto &[next, distance] : bfs(valve)) {
            if (graph[next].flowRate) {
                bestFromHere =
                    std::max(bestFromHere, bt(next, minutesLeft - distance));
            }
        }

        graph[valve].flowRate = flowRate;
        return bestFromHere + ifWeOpen;
    }

    struct Position {
        std::string where;
        int timeLeft;

        operator std::string() { return timeLeft > 0 ? where : "AA"; }
        operator int() { return timeLeft; }
    };

    int bt2(Position me, Position el) {
        if (me.timeLeft <= 0 && el.timeLeft <= 0)
            return 0;

        std::cout << "Me: " << me.where << " Elephant: " << el.where
                  << std::endl;

        int ifWeOpen = 0;

        auto openValve = [&](auto &who) {
            if (who.timeLeft <= 0)
                return 0;
            auto &where = graph[who];
            if (where.flowRate == 0)
                return 0;
            who.timeLeft--;
            assert(where.flowRate != 0);
            int result                 = who.timeLeft * where.flowRate;
            graph[where.name].flowRate = 0;
            assert(where.flowRate == 0);
            return result;
        };

        ifWeOpen += openValve(me);
        ifWeOpen += openValve(el);

        std::vector<std::string> opened;
        for (auto &[k, v] : graph) {
            if (v.flowRate) {
                opened.push_back(k);
            }
        }
        std::sort(opened.begin(), opened.end(), [&](auto &a, auto &b) {
            return graph[a].flowRate > graph[b].flowRate;
        });

        // otherwise need to handle case = 1
        assert(opened.size() % 2 == 0);

        if (opened.size() < 2)
            return 0;

        // the greatest closest to me
        // the greatest closest to el
        // for each unique couple,
        auto calc = [&](auto &who, auto &where) {
            int timeLeft = who.timeLeft - bfs(who)[where.name] - 1;
            return timeLeft * where.flowRate;
        };

        int bestSoFar = 0;
        auto &forMe   = opened[0];
        auto &forEl   = opened[1];

        for (int i = 0; i < opened.size() - 1; ++i) {
            auto first = graph[opened[i]];
            for (int j = i + 1; j < opened.size(); ++j) {
                auto second = graph[opened[j]];
                assert(first.flowRate >= second.flowRate);

                int ifMeGoesToFirst = (calc(me, first) + calc(el, second));
                int ifElGoesToFirst = (calc(el, first) + calc(me, second));

                if (ifMeGoesToFirst > bestSoFar
                    && ifMeGoesToFirst > ifElGoesToFirst) {
                    forMe     = first.name;
                    forEl     = second.name;
                    bestSoFar = ifMeGoesToFirst;
                } else if (ifElGoesToFirst > bestSoFar
                           && ifElGoesToFirst > ifMeGoesToFirst) {
                    forEl     = first.name;
                    forMe     = second.name;
                    bestSoFar = ifElGoesToFirst;
                }
            }
        }

        int bestFromHere = bt2({forMe, me.timeLeft - bfs(me)[forMe]},
                               {forEl, el.timeLeft - bfs(el)[forEl]});

        // graph[me].flowRate = flowRateMe;
        // graph[el].flowRate = flowRateEl;
        return bestFromHere + ifWeOpen;
    }

    int part1() {
        // find all open and distance from here
        // try each open
        // all turned off stop
        return bt("AA", 30);
    }

    int part2() { return bt2({"AA", 26}, {"AA", 26}); }
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
