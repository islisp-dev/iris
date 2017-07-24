package main

import (
	"io"
	"strings"

	"github.com/k0kubun/pp"
)

func main() {
	src := "(display #\\a) (println 10)"
	r := NewReader(strings.NewReader(src))
	for exp, err := r.ReadExp(); err != io.EOF; exp, err = r.ReadExp() {
		pp.Print(exp)
	}
}
