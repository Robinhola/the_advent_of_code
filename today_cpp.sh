year=$(date "+%Y")
day=$(date "+%d")

echo "$year\n$day\ncpp\n" | python3 ./bootstrap.py
