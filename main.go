package main

import (
	"io"
	"strings"

	"github.com/k0kubun/pp"
)

func main() {
	src := "(display #\\\\) (print 10)"
	r := strings.NewReader(src)
	for exp, err := Read(r); err != io.EOF; exp, err = Read(r) {
		pp.Print(exp)
	}
}
