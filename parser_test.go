package main

import (
	"strings"
	"testing"
)

func TestParse(t *testing.T) {
	r := NewReader(strings.NewReader("(print  #\\a) (cons 'a '(1 . 1.3E-10))"))
	for i := 0; i < 2; i++ {
		if _, err := Parse(r); err != nil {
			t.Error("Parser couldn't parse")
		}
	}
}
