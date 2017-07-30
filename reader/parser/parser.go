package parser

import (
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/ta2gch/gazelle/reader/tokenizer"
	"github.com/ta2gch/gazelle/runtime/class"
	"github.com/ta2gch/gazelle/runtime/class/cons"
)

var eop = class.New(class.Object, "End Of Parentheses")
var bod = class.New(class.Object, "Begin Of Dot")

func parseAtom(tok string) (class.Instance, class.Instance) {
	//
	// integer
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseInt(tok, 10, 64)
		return class.New(class.Integer, int(n)), nil
	}
	if r := regexp.MustCompile("^#[bB]([-+]?[01]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 2, 64)
		return class.New(class.Integer, int(n)), nil
	}
	if r := regexp.MustCompile("^#[oO]([-+]?[0-7]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 8, 64)
		return class.New(class.Integer, int(n)), nil
	}
	if r := regexp.MustCompile("^#[xX]([-+]?[[:xdigit:]]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 16, 64)
		return class.New(class.Integer, int(n)), nil
	}
	//
	// float
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+\\.[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseFloat(tok, 64)
		return class.New(class.Float, n), nil
	}
	if r := regexp.MustCompile("^([-+]?[[:digit:]]+(?:\\.[[:digit:]]+)?)[eE]([-+]?[[:digit:]]+)$").FindStringSubmatch(tok); len(r) >= 3 {
		n, _ := strconv.ParseFloat(r[1], 64)
		e, _ := strconv.ParseInt(r[2], 10, 64)
		return class.New(class.Float, n*math.Pow10(int(e))), nil
	}
	//
	// character
	//
	if m, _ := regexp.MatchString("^#\\\\newline$", tok); m {
		return class.New(class.Character, '\n'), nil
	}
	if m, _ := regexp.MatchString("^#\\\\space$", tok); m {
		return class.New(class.Character, ' '), nil
	}
	if r := regexp.MustCompile("^#\\\\([[:graph:]])$").FindStringSubmatch(tok); len(r) >= 2 {
		return class.New(class.Character, rune(r[1][0])), nil
	}
	//
	// string
	//
	if m, _ := regexp.MatchString("^\".*\"$", tok); m {
		return class.New(class.String, tok), nil
	}
	//
	// symbol
	//
	if "nil" == tok {
		return class.New(class.Null, nil), nil
	}
	if r := regexp.MustCompile("^:([<>/*=?_!$%[\\]^{}~0-9a-zA-Z]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		return class.New(class.Symbol, r[1]), nil
	}
	if m, _ := regexp.MatchString("^\\|.*\\|$", tok); m {
		return class.New(class.Symbol, tok), nil
	}
	if m, _ := regexp.MatchString("^[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*$", tok); m {
		return class.New(class.Symbol, tok), nil
	}
	return nil, class.New(class.ParseError, nil)
}

func parseMacro(tok string, t *tokenizer.Tokenizer) (class.Instance, class.Instance) {
	cdr, err := Parse(t)
	if err != nil {
		return nil, err
	}
	n := tok
	if m, _ := regexp.MatchString("#[[:digit:]]*[aA]", tok); m {
		s := class.New(class.Symbol, "array")
		i := strings.IndexRune(strings.ToLower(tok), 'a')
		if i == 1 {
			d := class.New(class.Integer, 1)
			return cons.New(s, cons.New(d, cons.New(cdr, class.New(class.Null, nil)))), nil
		}
		v, err := strconv.ParseInt(tok[1:i], 10, 32)
		if err != nil {
			return nil, class.New(class.ParseError, nil)
		}
		d := class.New(class.Integer, int(v))
		return cons.New(s, cons.New(d, cons.New(cdr, class.New(class.Null, nil)))), nil
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
	m := class.New(class.Symbol, n)
	return cons.New(m, cons.New(cdr, class.New(class.Null, nil))), nil
}
func parseCons(t *tokenizer.Tokenizer) (class.Instance, class.Instance) {
	car, err := Parse(t)
	if err == eop {
		return class.New(class.Null, nil), nil
	}
	if err == bod {
		cdr, err := Parse(t)
		if err != nil {
			return nil, err
		}
		if _, err := Parse(t); err != eop {
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
func Parse(t *tokenizer.Tokenizer) (class.Instance, class.Instance) {
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
		return nil, eop
	}
	if tok == "." {
		return nil, bod
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
