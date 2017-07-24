package main

import (
	"strings"
	"testing"
)

func TestReadToken(t *testing.T) {
	ans := []string{"(", "print", "#\\a", ")", "(", "cons", "'", "a", "'", "(", "1", ".", "1.3E-10", ")", ")"}
	re := NewReader(strings.NewReader("(print  #\\a) (cons 'a '(1 . 1.3E-10))"))
	for _, a := range ans {
		if b, err := re.ReadToken(); err != nil || a != b {
			t.Errorf("%s is not %s.", b, a)
		}
	}
}
