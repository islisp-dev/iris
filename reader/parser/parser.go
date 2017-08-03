package parser

import (
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/instance"

	"github.com/ta2gch/iris/reader/tokenizer"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

var eop = instance.NewSymbol("End Of Parentheses")
var bod = instance.NewSymbol("Begin Of Dot")

func parseAtom(tok string) (ilos.Instance, ilos.Instance) {
	//
	// integer
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseInt(tok, 10, 64)
		return instance.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[bB]([-+]?[01]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 2, 64)
		return instance.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[oO]([-+]?[0-7]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 8, 64)
		return instance.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[xX]([-+]?[[:xdigit:]]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 16, 64)
		return instance.NewInteger(int(n)), nil
	}
	//
	// float
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+\\.[[:digit:]]+$", tok); m {
		n, _ := strconv.ParseFloat(tok, 64)
		return instance.NewFloat(n), nil
	}
	if r := regexp.MustCompile("^([-+]?[[:digit:]]+(?:\\.[[:digit:]]+)?)[eE]([-+]?[[:digit:]]+)$").FindStringSubmatch(tok); len(r) >= 3 {
		n, _ := strconv.ParseFloat(r[1], 64)
		e, _ := strconv.ParseInt(r[2], 10, 64)
		return instance.NewFloat(n * math.Pow10(int(e))), nil
	}
	//
	// character
	//
	if m, _ := regexp.MatchString("^#\\\\newline$", tok); m {
		return instance.NewCharacter('\n'), nil
	}
	if m, _ := regexp.MatchString("^#\\\\space$", tok); m {
		return instance.NewCharacter(' '), nil
	}
	if r := regexp.MustCompile("^#\\\\([[:graph:]])$").FindStringSubmatch(tok); len(r) >= 2 {
		return instance.NewCharacter(rune(r[1][0])), nil
	}
	//
	// string
	//
	if m, _ := regexp.MatchString("^\".*\"$", tok); m {
		return instance.NewString(tok), nil
	}
	//
	// symbol
	//
	if "nil" == tok {
		return instance.NewNull(), nil
	}
	if r := regexp.MustCompile("^:([<>/*=?_!$%[\\]^{}~0-9a-zA-Z]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		return instance.NewSymbol(strings.ToUpper(r[0])), nil
	}
	if r := regexp.MustCompile("^&([<>/*=?_!$%[\\]^{}~0-9a-zA-Z]+)$").FindStringSubmatch(tok); len(r) >= 2 {
		return instance.NewSymbol(strings.ToUpper(r[0])), nil
	}
	if m, _ := regexp.MatchString("^\\|.*\\|$", tok); m {
		return instance.NewSymbol(tok), nil
	}
	if m, _ := regexp.MatchString("^[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*$", tok); m {
		return instance.NewSymbol(strings.ToUpper(tok)), nil
	}
	return nil, instance.NewParseError(instance.NewString(tok), class.Object)
}

func parseMacro(tok string, t *tokenizer.Tokenizer) (ilos.Instance, ilos.Instance) {
	cdr, err := Parse(t)
	if err != nil {
		return nil, err
	}
	n := tok
	if m, _ := regexp.MatchString("#[[:digit:]]*[aA]", tok); m {
		s := instance.NewSymbol("array")
		i := strings.IndexRune(strings.ToLower(tok), 'a')
		if i == 1 {
			d := instance.NewInteger(1)
			return instance.NewCons(s, instance.NewCons(d, instance.NewCons(cdr, instance.NewNull()))), nil
		}
		v, err := strconv.ParseInt(tok[1:i], 10, 32)
		if err != nil {
			return nil, instance.NewParseError(instance.NewString(tok), class.Integer)
		}
		d := instance.NewInteger(int(v))
		return instance.NewCons(s, instance.NewCons(d, instance.NewCons(cdr, instance.NewNull()))), nil
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
	m := instance.NewSymbol(n)
	return instance.NewCons(m, instance.NewCons(cdr, instance.NewNull())), nil
}
func parseCons(t *tokenizer.Tokenizer) (ilos.Instance, ilos.Instance) {
	car, err := Parse(t)
	if err == eop {
		return instance.NewNull(), nil
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
	return instance.NewCons(car, cdr), nil
}

// Parse builds a internal expression from tokens
func Parse(t *tokenizer.Tokenizer) (ilos.Instance, ilos.Instance) {
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
