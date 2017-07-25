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

func parseInteger(tok string) (*Object, error) {
	// TODO: http://minejima.jp/ISLispHyperDraft/islisp-v23.html#integer_class

	start := 0
	if tok[0] == '+' || tok[0] == '-' {
		start = 1
	}
	if tok[start] == '#' {
		base := 10
		if tok[start+1] == 'b' || tok[start+1] == 'B' {
			base = 2
		} else if tok[start+1] == 'o' || tok[start+1] == 'O' {
			base = 8
		} else if tok[start+1] == 'x' || tok[start+1] == 'X' {
			base = 16
		}
		if tok[start+2] == '+' || tok[start+2] == '-' {
			num, err := strconv.ParseInt(tok[start+3:], base, 32)
			if err != nil {
				return nil, err
			}
			if tok[start+2] == '-' {
				num = -num
			}
			return &Object{"integer", nil, nil, int(num)}, nil
		}
		num, err := strconv.ParseInt(tok[start+2:], base, 32)
		if err != nil {
			return nil, err
		}
		return &Object{"integer", nil, nil, int(num)}, nil
	}
	num, err := strconv.ParseInt(tok[start:], 10, 32)
	if err != nil {
		return nil, err
	}
	if tok[0] == '-' {
		num = -num
	}
	return &Object{"integer", nil, nil, int(num)}, nil
}

func parseFloat(tok string) (*Object, error) {
	e := strings.IndexRune(strings.ToUpper(tok), 'E')
	if e > 0 {
		num, err := strconv.ParseFloat(tok[:e], 32)
		if err != nil {
			return nil, err
		}
		exp, err := strconv.ParseFloat(tok[e+1:], 32)
		if err != nil {
			return nil, err
		}
		return &Object{"float", nil, nil, num * math.Pow(10.0, exp)}, nil
	}
	num, err := strconv.ParseFloat(tok, 32)
	if err != nil {
		return nil, err
	}
	return &Object{"float", nil, nil, num}, nil
}

func parseCharacter(tok string) (*Object, error) {
	if strings.ToLower(tok[2:]) == "newline" {
		return &Object{"character", nil, nil, '\n'}, nil
	}
	if strings.ToLower(tok[2:]) == "space" {
		return &Object{"character", nil, nil, ' '}, nil
	}
	if len(tok) != 3 {
		return nil, errors.New("Invalid character name")
	}
	return &Object{"character", nil, nil, rune(tok[2])}, nil
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

func parseAtom(tok string, t TokenReader) (*Object, error) {
	if matched, _ := regexp.MatchString("^(?:,@?|'|`|#[[:digit:]]*[aA])$", tok); matched {
		m, err := parseMacro(tok, t)
		if err != nil {
			return nil, err
		}
		return m, nil
	}
	if matched, _ := regexp.MatchString("^(?:[+-]?[[:digit:]]+|#[bB][+-]?[01]+|#[oO][0-7]+|#[xX][+-]?[[:xdigit:]]+)$", tok); matched {
		n, err := parseInteger(tok)
		if err != nil {
			return nil, err
		}
		return n, nil
	}
	if matched, _ := regexp.MatchString("^(?:[+-][[:digit:]]+(?:\\.[[:digit:]])?(?:[eE][-+]?[[:digit:]]+))?$", tok); matched {
		f, err := parseFloat(tok)
		if err != nil {
			return nil, err
		}
		return f, nil
	}
	if matched, _ := regexp.MatchString("^(?:#\\\\[[:graph:]]|#\\\\[[:alpha:]]+$", tok); matched {
		c, err := parseCharacter(tok)
		if err != nil {
			return nil, err
		}
		return c, nil
	}
	if matched, _ := regexp.MatchString("^\".*\"$", tok); matched {
		return &Object{"string", nil, nil, tok}, nil
	}
	if matched, _ := regexp.MatchString("^\\|.*\\|$", tok); matched {
		return &Object{"symbol", nil, nil, tok}, nil
	}
	if matched, _ := regexp.MatchString("[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*", tok); matched {
		return &Object{"symbol", nil, nil, strings.ToUpper(tok)}, nil
	}
	return nil, fmt.Errorf("Sorry, I could not parse %s", tok)
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
			return nil, errors.New("Invalid syntax")
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
	atom, err := parseAtom(tok, t)
	if err != nil {
		return nil, err
	}
	return atom, nil
}
