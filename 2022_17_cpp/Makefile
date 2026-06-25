test: main
	./main

input: main
	./main input

main: format main.cpp
	clang++ -std=c++20 -o main main.cpp

debug: format main.cpp
	clang++ -std=c++20 -g -o main main.cpp
	lldb main

format: main.cpp
	clang-format -i main.cpp
