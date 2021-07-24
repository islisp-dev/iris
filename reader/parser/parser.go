// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package parser

import (
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/islisp-dev/iris/reader/tokenizer"
	"github.com/islisp-dev/iris/runtime/env"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

var eop = instance.NewSymbol("End Of Parentheses")
var bod = instance.NewSymbol("Begin Of Dot")

func ParseAtom(tok *tokenizer.Token) (ilos.Instance, ilos.Instance) {
	str := tok.Str
	//
	// integer
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+$", str); m {
		n, _ := strconv.ParseInt(str, 10, 64)
		return instance.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[bB]([-+]?[01]+)$").FindStringSubmatch(str); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 2, 64)
		return instance.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[oO]([-+]?[0-7]+)$").FindStringSubmatch(str); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 8, 64)
		return instance.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[xX]([-+]?[[:xdigit:]]+)$").FindStringSubmatch(str); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 16, 64)
		return instance.NewInteger(int(n)), nil
	}
	//
	// float
	//
	if m, _ := regexp.MatchString(`^[-+]?[[:digit:]]+\.[[:digit:]]+$`, str); m {
		n, _ := strconv.ParseFloat(str, 64)
		return instance.NewFloat(n), nil
	}
	if r := regexp.MustCompile(`^([-+]?[[:digit:]]+(?:\.[[:digit:]]+)?)[eE]([-+]?[[:digit:]]+)$`).FindStringSubmatch(str); len(r) >= 3 {
		n, _ := strconv.ParseFloat(r[1], 64)
		e, _ := strconv.ParseInt(r[2], 10, 64)
		return instance.NewFloat(n * math.Pow10(int(e))), nil
	}
	//
	// character
	//
	if m, _ := regexp.MatchString(`^#\\newline$`, strings.ToLower(str)); m {
		return instance.NewCharacter('\n'), nil
	}
	if m, _ := regexp.MatchString(`^#\\space$`, strings.ToLower(str)); m {
		return instance.NewCharacter(' '), nil
	}
	if r := regexp.MustCompile(`^#\\([[:graph:]])$`).FindStringSubmatch(str); len(r) >= 2 {
		return instance.NewCharacter(rune(r[1][0])), nil
	}
	//
	// string
	//
	if r := regexp.MustCompile(`^"(.*)"$`).FindStringSubmatch(str); len(r) >= 2 {
		s := strings.Replace(r[1], "\\\\", "\\", -1)
		return instance.NewString([]rune(s)), nil
	}
	//
	// symbol
	//
	if str == "nil" {
		return instance.Nil, nil
	}
	re := `^(`
	re += `[:&][a-zA-Z]+|`
	re += `\|.*\||`
	re += `\+|-|1\+|1-|`
	re += `[a-zA-Z<>/*=?_!$%[\]^{}~][-a-zA-Z0-9+<>/*=?_!$%[\]^{}~]*|`
	re += `)$`
	if m, _ := regexp.MatchString(re, str); m {
		return instance.NewSymbol(strings.ToUpper(str)), nil
	}
	return nil, instance.Create(env.NewEnvironment(nil, nil, nil, nil),
		instance.ParseErrorClass,
		instance.NewSymbol("STRING"), instance.NewString([]rune(str)),
		instance.NewSymbol("EXPECTED-CLASS"), instance.ObjectClass)
}

func parseMacro(tok *tokenizer.Token, t *tokenizer.Reader) (ilos.Instance, ilos.Instance) {
	str := tok.Str
	cdr, err := Parse(t)
	if err != nil {
		return nil, err
	}
	n := str
	if m, _ := regexp.MatchString("#[[:digit:]]+[aA]", str); m {
		i := strings.IndexRune(strings.ToLower(str), 'a')
		var v int64 = 1
		if i != 1 {
			var err error
			v, err = strconv.ParseInt(str[1:i], 10, 64)
			if err != nil {
				return nil, instance.Create(env.NewEnvironment(nil, nil, nil, nil),
					instance.ParseErrorClass,
					instance.NewSymbol("STRING"), instance.NewString([]rune(str)),
					instance.NewSymbol("EXPECTED-CLASS"), instance.IntegerClass)
			}
		}
		if int(v) == 1 {
			return list2vector(cdr)
		}
		return list2array(int(v), cdr)
	}
	if str == "#" {
		return list2vector(cdr)
	}
	switch str {
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
func parseCons(t *tokenizer.Reader) (ilos.Instance, ilos.Instance) {
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
func Parse(t *tokenizer.Reader) (ilos.Instance, ilos.Instance) {
	tok, err := t.Next()
	if err != nil {
		return nil, instance.Create(env.NewEnvironment(nil, nil, nil, nil), instance.EndOfStreamClass)
	}
	str := tok.Str
	for (len(str) > 2 && str[:2] == "#|") || str[:1] == ";" {
		tok, err = t.Next()
		if err != nil {
			return nil, instance.Create(env.NewEnvironment(nil, nil, nil, nil), instance.EndOfStreamClass)
		}
		str = tok.Str
	}
	if str == "(" {
		cons, err := parseCons(t)
		if err != nil {
			return nil, err
		}
		return cons, err
	}
	if str == ")" {
		return nil, eop
	}
	if str == "." {
		return nil, bod
	}
	if mat, _ := regexp.MatchString("^(?:#'|,@?|'|`|#[[:digit:]]*[aA]|#)$", str); mat {
		m, err := parseMacro(tok, t)
		if err != nil {
			return nil, err
		}
		return m, nil
	}
	atom, err1 := ParseAtom(tok)
	if err1 != nil {
		return nil, err1
	}
	return atom, nil
}
