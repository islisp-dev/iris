package main

import (
	"errors"
	"io"
	"math"
	"regexp"
	"strconv"
	"strings"
)

var errEOP = errors.New("End Of Parentheses")

func newMatcher(src ...string) *regexp.Regexp {
	return regexp.MustCompile("^(?:" + strings.Join(src, ")$|^(?:") + ")$")
}

type tokens struct {
	eof     bool
	buf     string
	reader  io.RuneReader
	matcher *regexp.Regexp
}

var macroSyntax = ",@|,|'|`"
var integerSyntax = "[+-]?[[:digit:]]+|#[bB][01]+|#[oO][0-7]+|#[xX][[:xdigit:]]+"
var floatSyntax = "[+-]?[[:digit:]]+(?:.[[:digit:]]+|[eE][+-]?[[:digit:]]+|.[[:digit:]]+[eE][+-]?[[:digit:]]+)"
var characterSyntax = "#\\\\?|#\\\\(?:[[:alpha:]]+|[[:graph:]])"
var stringSyntax = "\"(?:\\\\\"|[^\"])*\"?"
var symbolSyntax = "[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*|\\|(?:\\\\\\||[^|])*\\|?"
var parenthesesSyntax = "\\(|\\)"

func tokenize(r io.RuneReader) *tokens {
	return &tokens{true, "", r, newMatcher(
		macroSyntax,
		integerSyntax,
		floatSyntax,
		characterSyntax,
		stringSyntax,
		symbolSyntax,
		parenthesesSyntax)}
}

func (t *tokens) next() (string, error) {
	for {
		r, _, err := t.reader.ReadRune()
		if !t.eof && err == io.EOF {
			t.eof = true
			return t.buf, nil
		}
		if err != nil {
			return t.buf, err
		}

		t.eof = false
		if t.matcher.MatchString(t.buf + string(r)) {
			t.buf += string(r)
		} else {
			if t.matcher.MatchString(t.buf) {
				result := t.buf
				t.buf = string(r)
				return result, nil
			} else if t.matcher.MatchString(string(r)) {
				t.buf = string(r)
			} else {
				t.buf = ""
			}
		}
	}
}

func parseCons(t *tokens) (*Object, error) {
	car, err := parse(t)
	if err != nil {
		if err == errEOP {
			return nil, nil
		}
		return nil, err
	}
	cdr, err := parseCons(t)
	if err != nil {
		return nil, err
	}
	return NewCons(car, cdr), nil
}

func parseInteger(tok string) (*Object, error) {
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

func parseAtom(tok string, t *tokens) (*Object, error) {
	if matched, _ := regexp.Match(macroSyntax, []byte(tok)); matched {
		cdr, err := parse(t)
		if err != nil {
			return nil, err
		}
		return NewCons(&Object{"symbol", nil, nil, tok}, NewCons(cdr, nil)), nil
	}
	if matched, _ := regexp.Match(integerSyntax, []byte(tok)); matched {
		n, err := parseInteger(tok)
		if err != nil {
			return nil, err
		}
		return n, nil
	}
	if matched, _ := regexp.Match(floatSyntax, []byte(tok)); matched {
		f, err := parseFloat(tok)
		if err != nil {
			return nil, err
		}
		return f, nil
	}
	if matched, _ := regexp.Match(characterSyntax, []byte(tok)); matched {
		c, err := parseCharacter(tok)
		if err != nil {
			return nil, err
		}
		return &Object{"character", nil, nil, c}, nil
	}
	if matched, _ := regexp.Match(stringSyntax, []byte(tok)); matched {
		return &Object{"string", nil, nil, tok}, nil
	}
	if tok[0] != '|' {
		return &Object{"symbol", nil, nil, strings.ToUpper(tok)}, nil
	}
	return &Object{"symbol", nil, nil, tok}, nil
}

func parse(t *tokens) (*Object, error) {
	tok, err := t.next()
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
	atom, err := parseAtom(tok, t)
	if err != nil {
		return nil, err
	}
	return atom, nil
}

func Read(r io.RuneReader) (*Object, error) {
	exp, err := parse(tokenize(r))
	return exp, err
}
