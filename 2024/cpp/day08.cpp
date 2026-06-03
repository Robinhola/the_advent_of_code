#include "day08.h"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <queue>
#include <ranges>
#include <sstream>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace Day08_utils {
using Node          = std::string;
using Children      = std::pair<Node, Node>;
using Tree          = std::unordered_map<Node, Children>;
using Number        = long long;
using NumberOfSteps = std::unordered_map<Node, Number>;

std::ostream &operator<<(std::ostream &os, const Tree &tree) {
    for (const auto &[node, children] : tree) {
        os << "Node: " << node << " -> Children: " << children.first << ", "
           << children.second << std::endl;
    }
    return os;
}

std::ostream &operator<<(std::ostream &os, const NumberOfSteps &tree) {
    for (const auto &[node, steps] : tree) {
        os << "Node: " << node << " -> number of steps: " << steps << std::endl;
    }
    return os;
}

std::ostream &operator<<(std::ostream &os, const std::vector<Node> &nodes) {
    for (const auto &node : nodes) {
        os << node << " ";
    }
    return os;
}
struct Input {
    const std::string direction;
    const Tree        mapping;

    Input(const std::string &direction, const Tree &mapping)
        : direction(direction), mapping(mapping) {}

    static Input from_stdin() {
        std::string direction;
        std::cin >> direction;
        Tree        tree;
        std::string line;
        while (std::getline(std::cin, line)) {
            if (line.empty()) {
                continue;
            }
            Node               node;
            std::string        children_str;
            std::istringstream iss(line);
            std::getline(iss, node, '=');
            std::getline(iss, children_str);
            node.erase(std::remove_if(node.begin(), node.end(), isspace),
                       node.end());
            children_str.erase(
                std::remove_if(
                    children_str.begin(), children_str.end(),
                    [](char c) { return isspace(c) || c == '(' || c == ')'; }),
                children_str.end());
            std::istringstream children_stream(children_str);
            Node               left, right;
            std::getline(children_stream, left, ',');
            std::getline(children_stream, right);
            tree[node] = {left, right};
        }
        return Input{direction, tree};
    }
};

struct Generator {
    size_t            position = 0;
    const std::string instructions;

    Generator(const std::string &instructions) : instructions(instructions) {}

    char next() {
        char c   = instructions[position];
        position = (position + 1) % instructions.length();
        return c;
    }
};

std::string part1(const Input &input) {
    Generator next_direction = Generator(input.direction);
    Node      current        = "AAA";
    Number    count          = 0;
    while (current != "ZZZ") {
        Children candidates = input.mapping.at(current);
        if (next_direction.next() == 'L') {
            current = candidates.first;
        } else {
            current = candidates.second;
        }
        count += 1;
    }
    std::ostringstream result;
    result << "Part1: " << count;
    return result.str();
}

std::string part2(const Input &input) {
    Generator        next_direction = Generator(input.direction);
    NumberOfSteps    numberOfSteps  = NumberOfSteps();
    std::queue<Node> current;
    for (const auto [node, children] : input.mapping) {
        if (node.ends_with('A')) {
            std::cout << "Node: " << node << std::endl;
            current.emplace(node);
        }
    }
    Number count = 0;
    while (!current.empty()) {
        char             direction = next_direction.next();
        std::queue<Node> next_elements;
        while (!current.empty()) {
            const auto &node = current.back();
            std::cout << "Node: " << node << std::endl;
            current.pop();
            if (node == "XXX") {
                throw std::invalid_argument("Reached a dead end");
            }
            if (node.ends_with('Z')) {
                numberOfSteps[node] = count;
                continue;
            }
            Node     next_node;
            Children candidates = input.mapping.at(node);
            if (direction == 'L') {
                next_node = candidates.first;
            } else {
                next_node = candidates.second;
            }
            next_elements.emplace(next_node);
        }
        count++;
        current = next_elements;
    }
    std::cout << numberOfSteps << std::endl;
    Number result = 1;
    for (const auto &[node, steps] : numberOfSteps) {
        result = std::lcm(result, steps);
    }
    std::ostringstream os;
    os << "Part2: " << result << " count: " << count;
    return os.str();
}
} // namespace Day08_utils

using namespace Day08_utils;

std::string Day08::solve() {
    Input input = Input::from_stdin();
    std::cout << "direction: " << input.direction << std::endl;
    std::cout << "tree: " << input.mapping << std::endl;
    std::ostringstream result;
    // result << part1(input) << std::endl;
    result << part2(input) << std::endl;
    return result.str();
}
