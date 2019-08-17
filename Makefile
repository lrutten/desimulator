all: build

build:
	ponyc

build-release:
	ponyc --config=release

run: ./desimulator
	./desimulator

docs:
	ponyc --docs
	cd desimulator-docs; mkdocs serve

clean:
	rm -vf desimulator
	rm -vfR desimulator-docs

