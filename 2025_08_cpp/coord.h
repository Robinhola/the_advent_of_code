#pragma once
#include <iostream>

struct Coord {
    int x;
    int y;
    int z;
};

inline long long distance(const Coord &a, const Coord &b) {
    using T = long long;
    T dx = a.x - b.x;
    T dy = a.y - b.y;
    T dz = a.z - b.z;
    return dx * dx + dy * dy + dz * dz;
}

inline std::istream &operator>>(std::istream &is, Coord &c) {
    char sep;
    is >> c.x >> sep >> c.y >> sep >> c.z;
    return is;
}

inline std::ostream &operator<<(std::ostream &os, const Coord &c) {
    return os << c.x << "|" << c.y << "|" << c.z;
}
