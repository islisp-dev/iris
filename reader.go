package main

import (
	"errors"
	"math"
	"regexp"
	"strconv"
	"strings"
)

var errEOP = errors.New("End Of Parentheses")

func ParseCons(t *Reader) (*Object, error) {
	car, err := Parse(t)
	if err == errEOP {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	cdr, err := ParseCons(t)
	if err != nil {
		return nil, err
	}
	return NewCons(car, cdr), nil
}

func ParseInteger(tok string) (*Object, error) {
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
		num, err := strconv.ParseInt(tok[start+2:], base, 32)
		if err != nil {
			return nil, err
		}
		if tok[0] == '-' {
			num = -num
		}
		return &Object{"integer", nil, nil, num}, nil
	}
	num, err := strconv.ParseInt(tok, 10, 32)
	if err != nil {
		return nil, err
	}
	return &Object{"integer", nil, nil, num}, nil
}

func ParseFloat(tok string) (*Object, error) {
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

func ParseCharacter(tok string) (*Object, error) {
	if strings.ToLower(tok) == "newline" {
		return &Object{"character", nil, nil, '\n'}, nil
	}
	if strings.ToLower(tok) == "space" {
		return &Object{"character", nil, nil, ' '}, nil
	}
	if len(tok) != 3 {
		return nil, errors.New("Invalid character name")
	}
	return &Object{"character", nil, nil, tok[2]}, nil
}

func ParseAtom(tok string, t *Reader) (*Object, error) {
	if matched, _ := regexp.Match(MacroToken, []byte(tok)); matched {
		cdr, err := Parse(t)
		if err != nil {
			return nil, err
		}
		return NewCons(&Object{"symbol", nil, nil, tok}, NewCons(cdr, nil)), nil
	}
	if matched, _ := regexp.Match(IntegerToken, []byte(tok)); matched {
		n, err := ParseInteger(tok)
		if err != nil {
			return nil, err
		}
		return n, nil
	}
	if matched, _ := regexp.Match(FloatToken, []byte(tok)); matched {
		f, err := ParseFloat(tok)
		if err != nil {
			return nil, err
		}
		return f, nil
	}
	if matched, _ := regexp.Match(CharacterToken, []byte(tok)); matched {
		c, err := ParseCharacter(tok)
		if err != nil {
			return nil, err
		}
		return c, nil
	}
	if matched, _ := regexp.Match(StringToken, []byte(tok)); matched {
		return &Object{"string", nil, nil, tok}, nil
	}
	return &Object{"symbol", nil, nil, tok}, nil
}

func Parse(t *Reader) (*Object, error) {
	tok, err := t.ReadToken()
	if err != nil {
		return nil, err
	}
	if tok == "(" {
		cons, err := ParseCons(t)
		if err != nil {
			return nil, err
		}
		return cons, err
	}
	if tok == ")" {
		return nil, errEOP
	}
	atom, err := ParseAtom(tok, t)
	if err != nil {
		return nil, err
	}
	return atom, nil
}

type ExpReader interface {
	ReadExp() (*Object, error)
}

func (r *Reader) ReadExp() (*Object, error) {
	exp, err := Parse(r)
	return exp, err
}
