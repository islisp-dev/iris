package parser

import (
	"errors"
	"fmt"
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/ta2gch/gazelle/core/class"
	"github.com/ta2gch/gazelle/core/class/cons"
	"github.com/ta2gch/gazelle/reader/tokenizer"
)

var errEOP = errors.New("End Of Parentheses")
var errBOD = errors.New("Begin Of Dot")

func parseAtom(tok string) (*class.Instance, error) {
	//
	// integer
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseInt(tok, 10, 64)
		return class.Integer.New(int(n)), nil
	}
	if r := regexp.MustCompile("^#[bB]([-+]?[01]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 2, 64)
		return class.Integer.New(int(n)), nil
	}
	if r := regexp.MustCompile("^#[oO]([-+]?[0-7]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 8, 64)
		return class.Integer.New(int(n)), nil
	}
	if r := regexp.MustCompile("^#[xX]([-+]?[[:xdigit:]]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 16, 64)
		return class.Integer.New(int(n)), nil
	}
	//
	// float
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+\\.[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseFloat(tok, 64)
		return class.Float.New(n), nil
	}
	if r := regexp.MustCompile("^([-+]?[[:digit:]]+(?:\\.[[:digit:]]+)?)[eE]([-+]?[[:digit:]]+)$").FindStringSubmatch(tok); len(r) >= 3 {
		n, _ := strconv.ParseFloat(r[1], 64)
		e, _ := strconv.ParseInt(r[2], 10, 64)
		return class.Float.New(n * math.Pow10(int(e))), nil
	}
	//
	// character
	//
	if m, _ := regexp.MatchString("^#\\\\newline$", tok); m {
		return class.Character.New('\n'), nil
	}
	if m, _ := regexp.MatchString("^#\\\\space$", tok); m {
		return class.Character.New(' '), nil
	}
	if r := regexp.MustCompile("^#\\\\([[:graph:]])$").FindStringSubmatch(tok); len(r) >= 2 {
		return class.Character.New(rune(r[1][0])), nil
	}
	//
	// string
	//
	if m, _ := regexp.MatchString("^\".*\"$", tok); m {
		return class.String.New(tok), nil
	}
	//
	// symbol
	//
	if "nil" == tok {
		return class.Null.New(nil), nil
	}
	if r := regexp.MustCompile("^:([<>/*=?_!$%[\\]^{}~0-9a-zA-Z]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		return class.Symbol.New(r[1]), nil
	}
	if m, _ := regexp.MatchString("^\\|.*\\|$", tok); m {
		return class.Symbol.New(tok), nil
	}
	if m, _ := regexp.MatchString("^[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*$", tok); m {
		return class.Symbol.New(tok), nil
	}
	return nil, fmt.Errorf("Sorry, I could not parse %s", tok)
}
func parseMacro(tok string, t *tokenizer.Tokenizer) (*class.Instance, error) {
	cdr, err := Parse(t)
	if err != nil {
		return nil, err
	}
	n := tok
	if m, _ := regexp.MatchString("#[[:digit:]]*[aA]", tok); m {
		s := class.Symbol.New("array")
		i := strings.IndexRune(strings.ToLower(tok), 'a')
		if i == 1 {
			d := class.Integer.New(1)
			return cons.New(s, cons.New(d, cons.New(cdr, class.Null.New(nil)))), nil
		}
		v, err := strconv.ParseInt(tok[1:i], 10, 32)
		if err != nil {
			return nil, err
		}
		d := class.Integer.New(int(v))
		return cons.New(s, cons.New(d, cons.New(cdr, class.Null.New(nil)))), nil
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
	m := class.Symbol.New(n)
	return cons.New(m, cons.New(cdr, class.Null.New(nil))), nil
}
func parseCons(t *tokenizer.Tokenizer) (*class.Instance, error) {
	car, err := Parse(t)
	if err == errEOP {
		return class.Null.New(nil), nil
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
	return cons.New(car, cdr), nil
}

// Parse builds a internal expression from tokens
func Parse(t *tokenizer.Tokenizer) (*class.Instance, error) {
	tok, err := t.Next()
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
