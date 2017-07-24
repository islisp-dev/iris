package main

import (
	"errors"
	"math"
	"regexp"
	"strconv"
	"strings"
)

var errEOP = errors.New("End Of Parentheses")
var errDOT = errors.New("Dot Cons")

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

func ParseArray(tok string) (*Object, error) {
	a := &Object{"symbol", nil, nil, "array"}
	d := &Object{"integer", nil, nil, 0}
	i := strings.IndexRune(strings.ToLower(tok), 'a')
	if i == 1 {
		d.Val = 1
	} else {
		v, err := strconv.Atoi(tok[1:i])
		if err != nil {
			return nil, err
		}
		d.Val = v
	}
	return NewCons(a, NewCons(d, NewCons(nil, nil))), nil
}

func ParseMacro(tok string, t *Reader) (*Object, error) {
	cdr, err := Parse(t)
	if err != nil {
		return nil, err
	}
	if matched, _ := regexp.MatchString("#[[:digit]]*[aA]", tok); matched {
		a, err := ParseArray(tok)
		if err != nil {
			return nil, err
		}
		a.Cdr.Cdr.Car = cdr
		return a, nil
	}
	m := &Object{"symbol", nil, nil, tok}
	return NewCons(m, NewCons(cdr, nil)), nil
}

func ParseAtom(tok string, t *Reader) (*Object, error) {
	if matched, _ := regexp.MatchString(MacroToken, tok); matched {
		m, err := ParseMacro(tok, t)
		if err != nil {
			return nil, err
		}
		return m, nil
	}
	if matched, _ := regexp.MatchString(IntegerToken, tok); matched {
		n, err := ParseInteger(tok)
		if err != nil {
			return nil, err
		}
		return n, nil
	}
	if matched, _ := regexp.MatchString(FloatToken, tok); matched {
		f, err := ParseFloat(tok)
		if err != nil {
			return nil, err
		}
		return f, nil
	}
	if matched, _ := regexp.MatchString(CharacterToken, tok); matched {
		c, err := ParseCharacter(tok)
		if err != nil {
			return nil, err
		}
		return c, nil
	}
	if matched, _ := regexp.MatchString(StringToken, tok); matched {
		return &Object{"string", nil, nil, tok}, nil
	}
	return &Object{"symbol", nil, nil, tok}, nil
}

func ParseCons(t *Reader) (*Object, error) {
	car, err := Parse(t)
	if err == errEOP {
		return nil, nil
	}
	if err == errDOT {
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
	cdr, err := ParseCons(t)
	if err != nil {
		return nil, err
	}
	return NewCons(car, cdr), nil
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
	if tok == "." {
		return nil, errDOT
	}
	atom, err := ParseAtom(tok, t)
	if err != nil {
		return nil, err
	}
	return atom, nil
}
