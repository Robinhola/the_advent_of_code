year=$(date "+%Y")
day=$(date "+%d")

echo "$year\n$day\nocaml\n" | python3 ./bootstrap.py
