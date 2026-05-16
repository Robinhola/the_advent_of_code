run:
	python3 main.py

test:
	pytest main.py

with_input: ./input
	cat ./input | python3 main.py

format:
	isort .
	black .

fmt: format
