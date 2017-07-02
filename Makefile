all: build

parser.go: parser.go.y
	goyacc -o $@ $<

.PHONY: build clean format run vet test commit

build: parser.go
	go build

clean: format
	go clean
	rm -rf parser.go *~

format:
	go fmt

run: parser.go
	go run

vet: parser.go
	go vet

test: parser.go vet
	go test

commit: clean
	git commit -a
