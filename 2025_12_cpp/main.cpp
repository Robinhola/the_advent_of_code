#include <cassert>
#include <iostream>

#include "args.h"

using namespace std;

pair<int, int> rotate(int x, int y, int rotation) {
    if (rotation == 0) return {x, y};
    if (rotation == 1) return {2 - y, x};

    auto [rx, ry] = rotate(x, y, 1);
    return rotate(rx, ry, rotation - 1);
}

pair<int, int> flip(int x, int y) {
    return {2 - x, y};
}

bool canPlace(vector<vector<char>> &grid, const Shape &shape, int rotation, bool flipped, int x, int y, int nx, int ny) {
    if (ny >= 3) return true;
    if (nx >= 3) return canPlace(grid, shape, rotation, flipped, x, y, 0, ny + 1);

    auto [rx, ry] = rotate(x, y, rotation);
    auto [fx, fy] = flipped ? flip(rx, ry) : make_pair(rx, ry);

    if (shape[ny][nx] == '.') {
        return canPlace(grid, shape, rotation, flipped, x, y, nx + 1, ny);
    }
    if (shape[ny][nx] == '#' && grid[fy][fx] == '.') {
        grid[fy][fx] = '#';
        auto result = canPlace(grid, shape, rotation, flipped, x, y, nx + 1, ny);
        if (!result) grid[fy][fx] = '.';
        return result;
    }
    assert(false);
}

void reinitShape(vector<vector<char>> &grid, int x, int y) {
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            grid[y + i][x + j] = '.';
        }
    }
}

bool canPlace(vector<vector<char>> &grid, int shape, int x, int y) {
    if (shape >= 6) return true;

    // for each x, y try placing the shape, if yes, go to next shape

    // for each rotation
    for (int rotation = 0; rotation < 4; ++rotation) {


    }

    return false;
}

struct Parts {
    int part1() { return 0; }
    int part2() { return 0; }
};

int main(int argc, char *argv[]) {
    auto args = Args::parse(argc, argv);
    Input inp = parse(*args.input);

    Parts p{};
    cout << p.part1() << endl;
    cout << p.part2() << endl;
}
