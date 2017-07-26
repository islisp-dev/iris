package main

import (
	"errors"
	"fmt"
	"math"
	"regexp"
	"strconv"
	"strings"
)

var errEOP = errors.New("End Of Parentheses")
var errBOD = errors.New("Begin Of Dot")

func parseAtom(tok string) (*Object, error) {
	//
	// integer
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseInt(tok, 10, 64)
		return &Object{"integer", nil, nil, int(n)}, nil
	}
	if r := regexp.MustCompile("^#[bB]([-+]?[01]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 2, 64)
		return &Object{"integer", nil, nil, int(n)}, nil
	}
	if r := regexp.MustCompile("^#[oO]([-+]?[0-7]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 8, 64)
		return &Object{"integer", nil, nil, int(n)}, nil
	}
	if r := regexp.MustCompile("^#[xX]([-+]?[[:xdigit:]]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 16, 64)
		return &Object{"integer", nil, nil, int(n)}, nil
	}
	//
	// float
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+\\.[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseFloat(tok, 64)
		return &Object{"float", nil, nil, n}, nil
	}
	if r := regexp.MustCompile("^([-+]?[[:digit:]]+(?:\\.[[:digit:]]+)?)[eE]([-+]?[[:digit:]]+)$").FindStringSubmatch(tok); len(r) >= 3 {
		n, _ := strconv.ParseFloat(r[1], 64)
		e, _ := strconv.ParseInt(r[2], 10, 64)
		return &Object{"float", nil, nil, n * math.Pow10(int(e))}, nil
	}
	//
	// character
	//
	if m, _ := regexp.MatchString("^#\\\\newline$", tok); m {
		return &Object{"character", nil, nil, '\n'}, nil
	}
	if m, _ := regexp.MatchString("^#\\\\space$", tok); m {
		return &Object{"character", nil, nil, ' '}, nil
	}
	if r := regexp.MustCompile("^#\\\\([[:graph:]])$").FindStringSubmatch(tok); len(r) >= 2 {
		return &Object{"character", nil, nil, rune(r[1][0])}, nil
	}
	//
	// string
	//
	if m, _ := regexp.MatchString("^\".*\"$", tok); m {
		return &Object{"string", nil, nil, tok}, nil
	}
	//
	// symbol
	//
	if m, _ := regexp.MatchString("^\\|.*\\|$", tok); m {
		return &Object{"symbol", nil, nil, tok}, nil
	}
	if m, _ := regexp.MatchString("^[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*$", tok); m {
		return &Object{"symbol", nil, nil, strings.ToUpper(tok)}, nil
	}
	return nil, fmt.Errorf("Sorry, I could not parse %s", tok)
}
func parseMacro(tok string, t TokenReader) (*Object, error) {
	cdr, err := Parse(t)
	if err != nil {
		return nil, err
	}
	n := tok
	if m, _ := regexp.MatchString("#[[:digit:]]*[aA]", tok); m {
		s := &Object{"symbol", nil, nil, "array"}
		d := &Object{"integer", nil, nil, 0}
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
		return NewCons(s, NewCons(d, NewCons(cdr, nil))), nil
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
	m := &Object{"symbol", nil, nil, n}
	return NewCons(m, NewCons(cdr, nil)), nil
}
func parseCons(t TokenReader) (*Object, error) {
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
	return NewCons(car, cdr), nil
}

// Parse builds a internal expression from tokens
func Parse(t TokenReader) (*Object, error) {
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
