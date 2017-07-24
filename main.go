package main

import (
	"io"
	"strings"

	"github.com/k0kubun/pp"
)

func main() {
	src := "(display . print)"
	r := NewReader(strings.NewReader(src))
	for exp, err := r.ReadExp(); err != io.EOF; exp, err = r.ReadExp() {
		pp.Print(exp)
	}
}
