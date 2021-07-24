all:
	go build .
test:
	go test -cover ./...
