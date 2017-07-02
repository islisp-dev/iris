all: parser.go

parser.go: parser.go.y
	goyacc -o $@ $<

