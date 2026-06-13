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
        graph.reserve(valves.size());
        for (auto &v : valves) {
            auto key = v.name;
            if (v.flowRate)
                open++;
            graph.emplace(key, std::move(v));
        }
        valves.clear();
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
        int nextMinuteCanDoSomething;
    };

    std::vector<std::string> getAllOpened() {
        std::vector<std::string> opened;
        for (auto &[k, v] : graph) {
            if (v.flowRate)
                opened.push_back(k);
        }
        return opened;
    }

    int bt2(Position me, Position el, int timeLeft) {
        if (timeLeft <= 0)
            return 0;

        std::vector<std::string> opened = getAllOpened();
        if (opened.empty())
            return 0;

        // for me && el if they can do something open and move to the next one
        auto openValve = [&](Position &who, std::string closest) {
            if (who.nextMinuteCanDoSomething < timeLeft
                || graph[closest].flowRate == 0)
                return 0;

            int distance = bfs(who.where)[closest];
            if (distance + 1 < timeLeft) {
                who.nextMinuteCanDoSomething = timeLeft - distance - 1;
                who.where                    = closest;
            } else {
                who.nextMinuteCanDoSomething = 0;
            }

            int flowRate              = graph[who.where].flowRate;
            graph[who.where].flowRate = 0;

            return who.nextMinuteCanDoSomething * flowRate;
        };

        int bestSoFar = 0;
        for (auto closest : opened) {
            Position backupMe = me;
            Position backupEl = el;
            int flowRate      = graph[closest].flowRate;

            if (flowRate == 0)
                continue;

            int ifWeTakeThisOne =
                openValve(me, closest) + openValve(el, closest);
            int nextInterestingTime = std::max(me.nextMinuteCanDoSomething,
                                               el.nextMinuteCanDoSomething);

            bestSoFar = std::max(
                bestSoFar, ifWeTakeThisOne + bt2(me, el, nextInterestingTime));

            assert(graph[closest].flowRate == 0 || ifWeTakeThisOne == 0);
            graph[closest].flowRate = flowRate;
            me                      = backupMe;
            el                      = backupEl;
        }

        return bestSoFar;
    }

    int part1() {
        // find all open and distance from here
        // try each open
        // all turned off stop
        return bt("AA", 30);
    }

    int part2() { return bt2({"AA", 26}, {"AA", 26}, 26); }
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
