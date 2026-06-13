test: main
	./main

input: main
	./main input

main: format main.cpp
	clang++ -std=c++20 -g -o main main.cpp

debug: format main.cpp
	clang++ -std=c++20 -g -o main main.cpp

lldb: debug
	lldb main

perf: format main.cpp
	clang++ -std=c++20 -g -O3 -o main main.cpp
	xcrun xctrace record --template "Time Profiler" --launch main

format: main.cpp
	clang-format -i main.cpp
