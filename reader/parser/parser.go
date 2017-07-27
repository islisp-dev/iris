package parser

import (
	"errors"
	"fmt"
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/ta2gch/gazelle/reader/tokenizer"
	"github.com/ta2gch/gazelle/runtime/class"
	"github.com/ta2gch/gazelle/runtime/object"
)

var errEOP = errors.New("End Of Parentheses")
var errBOD = errors.New("Begin Of Dot")

// cons creates cons pair from two objects
func cons(car *object.Object, cdr *object.Object) *object.Object {
	if cdr == nil || cdr.Class == class.List {
		return &object.Object{class.List, car, cdr, nil}
	}
	return &object.Object{class.Cons, car, cdr, nil}
}

func parseAtom(tok string) (*object.Object, error) {
	//
	// integer
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseInt(tok, 10, 64)
		return &object.Object{class.Integer, nil, nil, int(n)}, nil
	}
	if r := regexp.MustCompile("^#[bB]([-+]?[01]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 2, 64)
		return &object.Object{class.Integer, nil, nil, int(n)}, nil
	}
	if r := regexp.MustCompile("^#[oO]([-+]?[0-7]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 8, 64)
		return &object.Object{class.Integer, nil, nil, int(n)}, nil
	}
	if r := regexp.MustCompile("^#[xX]([-+]?[[:xdigit:]]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 16, 64)
		return &object.Object{class.Integer, nil, nil, int(n)}, nil
	}
	//
	// float
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+\\.[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseFloat(tok, 64)
		return &object.Object{class.Float, nil, nil, n}, nil
	}
	if r := regexp.MustCompile("^([-+]?[[:digit:]]+(?:\\.[[:digit:]]+)?)[eE]([-+]?[[:digit:]]+)$").FindStringSubmatch(tok); len(r) >= 3 {
		n, _ := strconv.ParseFloat(r[1], 64)
		e, _ := strconv.ParseInt(r[2], 10, 64)
		return &object.Object{class.Float, nil, nil, n * math.Pow10(int(e))}, nil
	}
	//
	// character
	//
	if m, _ := regexp.MatchString("^#\\\\newline$", tok); m {
		return &object.Object{class.Character, nil, nil, '\n'}, nil
	}
	if m, _ := regexp.MatchString("^#\\\\space$", tok); m {
		return &object.Object{class.Character, nil, nil, ' '}, nil
	}
	if r := regexp.MustCompile("^#\\\\([[:graph:]])$").FindStringSubmatch(tok); len(r) >= 2 {
		return &object.Object{class.Character, nil, nil, rune(r[1][0])}, nil
	}
	//
	// string
	//
	if m, _ := regexp.MatchString("^\".*\"$", tok); m {
		return &object.Object{class.String, nil, nil, tok}, nil
	}
	//
	// symbol
	//
	if "nil" == strings.ToLower(tok) {
		return nil, nil
	}
	if r := regexp.MustCompile("^:([<>/*=?_!$%[\\]^{}~0-9a-zA-Z]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		return &object.Object{class.Symbol, nil, nil, r[1]}, nil
	}
	if m, _ := regexp.MatchString("^\\|.*\\|$", tok); m {
		return &object.Object{class.Symbol, nil, nil, tok}, nil
	}
	if m, _ := regexp.MatchString("^[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*$", tok); m {
		return &object.Object{class.Symbol, nil, nil, strings.ToUpper(tok)}, nil
	}
	return nil, fmt.Errorf("Sorry, I could not parse %s", tok)
}
func parseMacro(tok string, t *tokenizer.TokenReader) (*object.Object, error) {
	cdr, err := Parse(t)
	if err != nil {
		return nil, err
	}
	n := tok
	if m, _ := regexp.MatchString("#[[:digit:]]*[aA]", tok); m {
		s := &object.Object{class.Symbol, nil, nil, "array"}
		d := &object.Object{class.Integer, nil, nil, 0}
		i := strings.IndexRune(strings.ToLower(tok), 'a')
		if i == 1 {
			d.Val = 1
		} else {
			v, err := strconv.ParseInt(tok[1:i], 10, 32)
			if err != nil {
				return nil, err
			}
			d.Val = int(v)
		}
		return cons(s, cons(d, cons(cdr, nil))), nil
	}
	switch tok {
	case ",@":
		n = "commaat"
	case ",":
		n = "comma"
	case "'":
		n = "quote"
	case "`":
		n = "backquote"
	}
	m := &object.Object{class.Symbol, nil, nil, n}
	return cons(m, cons(cdr, nil)), nil
}
func parseCons(t *tokenizer.TokenReader) (*object.Object, error) {
	car, err := Parse(t)
	if err == errEOP {
		return nil, nil
	}
	if err == errBOD {
		cdr, err := Parse(t)
		if err != nil {
			return nil, err
		}
		if _, err := Parse(t); err != errEOP {
			return nil, err
		}
		return cdr, nil
	}
	if err != nil {
		return nil, err
	}
	cdr, err := parseCons(t)
	if err != nil {
		return nil, err
	}
	return cons(car, cdr), nil
}

// Parse builds a internal expression from tokens
func Parse(t *tokenizer.TokenReader) (*object.Object, error) {
	tok, err := t.ReadToken()
	if err != nil {
		return nil, err
	}
	if tok == "(" {
		cons, err := parseCons(t)
		if err != nil {
			return nil, err
		}
		return cons, err
	}
	if tok == ")" {
		return nil, errEOP
	}
	if tok == "." {
		return nil, errBOD
	}
	if mat, _ := regexp.MatchString("^(?:,@?|'|`|#[[:digit:]]*[aA])$", tok); mat {
		m, err := parseMacro(tok, t)
		if err != nil {
			return nil, err
		}
		return m, nil
	}
	atom, err := parseAtom(tok)
	if err != nil {
		return nil, err
	}
	return atom, nil
}
