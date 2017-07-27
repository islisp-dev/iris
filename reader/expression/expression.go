package expression

import (
	"github.com/ta2gch/gazelle/reader/parser"
	"github.com/ta2gch/gazelle/reader/tokenizer"
	"github.com/ta2gch/gazelle/runtime"
)

// ExpReader interface type is the interface for reading expressions.
type ExpReader struct {
	tokenizer.TokenReader
}

// ReadExp returns a expression that is a pointer to Object
func (r *ExpReader) ReadExp() (*runtime.Object, error) {
	exp, err := parser.Parse(r)
	return exp, err
}
