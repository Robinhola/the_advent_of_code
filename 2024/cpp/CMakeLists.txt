cmake_minimum_required(VERSION 3.10)
project(aoc-2024)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_BUILD_TYPE Debug)

set(FILES
    main.cpp
    dayXX.cpp
    day01.cpp
    day08.cpp
    day09.cpp
    day10.cpp
)

add_executable(aoc_2024.exe ${FILES})
