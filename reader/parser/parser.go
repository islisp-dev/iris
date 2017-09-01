// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package parser

import (
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/ta2gch/iris/reader/tokenizer"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

var eop = instance.NewSymbol("End Of Parentheses")
var bod = instance.NewSymbol("Begin Of Dot")

func ParseAtom(tok string) (ilos.Instance, ilos.Instance) {
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
	if m, _ := regexp.MatchString(`^[-+]?[[:digit:]]+\.[[:digit:]]+$`, tok); m {
		n, _ := strconv.ParseFloat(tok, 64)
		return instance.NewFloat(n), nil
	}
	if r := regexp.MustCompile(`^([-+]?[[:digit:]]+(?:\.[[:digit:]]+)?)[eE]([-+]?[[:digit:]]+)$`).FindStringSubmatch(tok); len(r) >= 3 {
		n, _ := strconv.ParseFloat(r[1], 64)
		e, _ := strconv.ParseInt(r[2], 10, 64)
		return instance.NewFloat(n * math.Pow10(int(e))), nil
	}
	//
	// character
	//
	if m, _ := regexp.MatchString(`^#\\newline$`, strings.ToLower(tok)); m {
		return instance.NewCharacter('\n'), nil
	}
	if m, _ := regexp.MatchString(`^#\\space$`, strings.ToLower(tok)); m {
		return instance.NewCharacter(' '), nil
	}
	if r := regexp.MustCompile(`^#\\([[:graph:]])$`).FindStringSubmatch(tok); len(r) >= 2 {
		return instance.NewCharacter(rune(r[1][0])), nil
	}
	//
	// string
	//
	if r := regexp.MustCompile(`^"(.*)"$`).FindStringSubmatch(tok); len(r) >= 2 {
		return instance.NewString(r[1]), nil
	}
	//
	// symbol
	//
	if "nil" == tok {
		return instance.Nil, nil
	}
	str := `^(`
	str += `[:&][a-zA-Z]+|`
	str += `\|.*\||`
	str += `\+|-|1\+|1-|`
	str += `[a-zA-Z<>/*=?_!$%[\]^{}~][-a-zA-Z0-9+<>/*=?_!$%[\]^{}~]*|`
	str += `)$`
	if m, _ := regexp.MatchString(str, tok); m {
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
	if m, _ := regexp.MatchString("#[[:digit:]]+[aA]", tok); m {
		i := strings.IndexRune(strings.ToLower(tok), 'a')
		var v int64 = 1
		if i != 1 {
			var err error
			v, err = strconv.ParseInt(tok[1:i], 10, 64)
			if err != nil {
				return nil, instance.NewParseError(instance.NewString(tok), class.Integer)
			}
		}
		return list2array(int(v), cdr)
	}
	if tok == "#" {
		return list2vector(cdr)
	}
	switch tok {
	case "#'":
		n = "FUNCTION"
	case ",@":
		n = "UNQUOTE-SPLICING"
	case ",":
		n = "UNQUOTE"
	case "'":
		n = "QUOTE"
	case "`":
		n = "QUASIQUOTE"
	}
	m := instance.NewSymbol(n)
	return instance.NewCons(m, instance.NewCons(cdr, instance.Nil)), nil
}
func parseCons(t *tokenizer.Tokenizer) (ilos.Instance, ilos.Instance) {
	car, err := Parse(t)
	if err == eop {
		return instance.Nil, nil
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
	for (len(tok) > 2 && tok[:2] == "#|") || tok[:1] == ";" {
		tok, err = t.Next()
		if err != nil {
			return nil, err
		}
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
	if mat, _ := regexp.MatchString("^(?:#'|,@?|'|`|#[[:digit:]]*[aA]|#)$", tok); mat {
		m, err := parseMacro(tok, t)
		if err != nil {
			return nil, err
		}
		return m, nil
	}
	atom, err := ParseAtom(tok)
	if err != nil {
		return nil, err
	}
	return atom, nil
}
