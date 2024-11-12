year=$(date "+%Y")
day=$(date "+%d")

echo "$year\n$day\npython\n" | python3 ./bootstrap.py
