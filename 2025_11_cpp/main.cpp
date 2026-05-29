#include <array>
#include <fstream>
#include <iostream>
#include <memory>
#include <ranges>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

const std::string sample = R"(aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out)";

const std::string sample2 = R"(svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out)";

struct Args {
  std::unique_ptr<std::istream> input;
  std::unique_ptr<std::istream> input2;

  static Args parse(int argc, char *argv[]) {
    if (argc > 1) {
      std::string path = argv[1];
      return {std::make_unique<std::ifstream>(path),
              std::make_unique<std::ifstream>(path)};
    }
    return {std::make_unique<std::istringstream>(sample),
            std::make_unique<std::istringstream>(sample2)};
  }
};

struct Line {
  std::string header;
  std::vector<std::string> values;
};

std::istream &operator>>(std::istream &is, Line &line) {
  std::string rest;
  std::getline(is, line.header, ':');
  std::getline(is, rest);
  std::istringstream ss(rest);
  line.values = {std::istream_iterator<std::string>(ss), {}};
  return is;
}

std::ostream &operator<<(std::ostream &os, const Line &line) {
  os << line.header << " | ";
  for (auto it = line.values.begin(); it != line.values.end(); ++it) {
    if (it != line.values.begin())
      os << ", ";
    os << *it;
  }
  return os;
}

struct G {
  const std::unordered_map<std::string, std::vector<std::string>> &graph;
  std::unordered_map<std::string, int> cache;
  std::unordered_map<std::string, std::array<std::array<long long, 2>, 2>>
      cache2;

  int operator()(const std::string &start, const std::string &target) {
    if (start == target)
      return 1;
    if (cache.count(start))
      return cache[start];
    int total = 0;
    for (auto &path : graph.at(start))
      total += (*this)(path, target);
    return cache[start] = total;
  }

  long long operator()(const std::string &start, const std::string &target,
                       bool seen_fft, bool seen_dac) {
    if (start == target) {
        return seen_fft && seen_dac ? 1 : 0;
    }

    seen_fft = seen_fft || start == "fft";
    seen_dac = seen_dac || start == "dac";

    if (!cache2.count(start))
      cache2[start].fill({-1, -1});

    auto &cached = cache2[start][seen_fft][seen_dac];
    if (cached != -1) {
      return cached;
    }

    long long total = 0;
    for (auto &path : graph.at(start)) {
      total += (*this)(path, target, seen_fft, seen_dac);
    }
    return cached = total;
  }
};

int main(int argc, char *argv[]) {
  auto args = Args::parse(argc, argv);

  std::unordered_map<std::string, std::vector<std::string>> graph;
  for (auto line : std::ranges::istream_view<Line>(*args.input))
    graph[line.header] = std::move(line.values);

  G g{graph};
  std::cout << g("you", "out") << std::endl;

  std::unordered_map<std::string, std::vector<std::string>> graph2;
  for (auto line : std::ranges::istream_view<Line>(*args.input2))
    graph2[line.header] = std::move(line.values);

  G g2{graph2};
  std::cout << g2("svr", "out", false, false) << std::endl;
}
