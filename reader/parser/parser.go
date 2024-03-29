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
	"github.com/islisp-dev/iris/core"
)

var eop = core.NewSymbol("End Of Parentheses")
var bod = core.NewSymbol("Begin Of Dot")

func ParseAtom(e core.Environment, tok *tokenizer.Token) (core.Instance, core.Instance) {
	str := tok.Str
	//
	// integer
	//
	if m, _ := regexp.MatchString("^[-+]?[[:digit:]]+$", str); m {
		n, _ := strconv.ParseInt(str, 10, 64)
		return core.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[bB]([-+]?[01]+)$").FindStringSubmatch(str); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 2, 64)
		return core.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[oO]([-+]?[0-7]+)$").FindStringSubmatch(str); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 8, 64)
		return core.NewInteger(int(n)), nil
	}
	if r := regexp.MustCompile("^#[xX]([-+]?[[:xdigit:]]+)$").FindStringSubmatch(str); len(r) >= 2 {
		n, _ := strconv.ParseInt(r[1], 16, 64)
		return core.NewInteger(int(n)), nil
	}
	//
	// float
	//
	if m, _ := regexp.MatchString(`^[-+]?[[:digit:]]+\.[[:digit:]]+$`, str); m {
		n, _ := strconv.ParseFloat(str, 64)
		return core.NewFloat(n), nil
	}
	if r := regexp.MustCompile(`^([-+]?[[:digit:]]+(?:\.[[:digit:]]+)?)[eE]([-+]?[[:digit:]]+)$`).FindStringSubmatch(str); len(r) >= 3 {
		n, _ := strconv.ParseFloat(r[1], 64)
		e, _ := strconv.ParseInt(r[2], 10, 64)
		return core.NewFloat(n * math.Pow10(int(e))), nil
	}
	//
	// character
	//
	if m, _ := regexp.MatchString(`^#\\newline$`, strings.ToLower(str)); m {
		return core.NewCharacter('\n'), nil
	}
	if m, _ := regexp.MatchString(`^#\\space$`, strings.ToLower(str)); m {
		return core.NewCharacter(' '), nil
	}
	if r := regexp.MustCompile(`^#\\([[:graph:]])$`).FindStringSubmatch(str); len(r) >= 2 {
		return core.NewCharacter(rune(r[1][0])), nil
	}
	//
	// string
	//
	if r := regexp.MustCompile(`^"(.*)"$`).FindStringSubmatch(str); len(r) >= 2 {
		s := strings.Replace(r[1], "\\\\", "\\", -1)
		return core.NewString([]rune(s)), nil
	}
	//
	// symbol
	//
	if str == "nil" || str == "NIL" {
		return core.Nil, nil
	}
	re := `^(`
	re += `[:&](?:[a-zA-Z]|-)+|`
	re += `\|.*\||`
	re += `\+|-|1\+|1-|`
	re += `[a-zA-Z<>/*=?_!$%[\]^{}~][-a-zA-Z0-9+<>/*=?_!$%[\]^{}~]*|`
	re += `)$`
	if m, _ := regexp.MatchString(re, str); m {
		return core.NewSymbol(strings.ToUpper(str), tok.Line, tok.Column), nil
	}
	return core.SignalCondition(
		e,
		core.NewParseError(
			e,
			core.NewString([]rune(str)),
			core.ObjectClass,
		),
		core.Nil,
	)
}

func parseMacro(e core.Environment, tok *tokenizer.Token, t *tokenizer.BufferedTokenReader) (core.Instance, core.Instance) {
	str := tok.Str
	cdr, err := Parse(e, t)
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
				return core.SignalCondition(
					e,
					core.NewParseError(
						e,
						core.NewString([]rune(str)),
						core.IntegerClass,
					),
					core.Nil,
				)
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
	m := core.NewSymbol(n, tok.Line, tok.Line)
	return core.NewCons(m, core.NewCons(cdr, core.Nil)), nil
}
func parseCons(e core.Environment, t *tokenizer.BufferedTokenReader) (core.Instance, core.Instance) {
	car, err := Parse(e, t)
	if err == eop {
		return core.Nil, nil
	}
	if err == bod {
		cdr, err := Parse(e, t)
		if err != nil {
			return nil, err
		}
		if _, err := Parse(e, t); err != eop {
			return nil, err
		}
		return cdr, nil
	}
	if err != nil {
		return nil, err
	}
	cdr, err := parseCons(e, t)
	if err != nil {
		return nil, err
	}
	return core.NewCons(car, cdr), nil
}

// Parse builds a internal expression from tokens
func Parse(e core.Environment, t *tokenizer.BufferedTokenReader) (core.Instance, core.Instance) {
	tok, err := t.ReadToken()
	if err != nil {
		return core.SignalCondition(e, core.NewEndOfStream(e), core.Nil)
	}
	str := tok.Str
	for (len(str) > 2 && str[:2] == "#|") || str[:1] == ";" {
		tok, err = t.ReadToken()
		if err != nil {
			return core.SignalCondition(e, core.NewEndOfStream(e), core.Nil)
		}
		str = tok.Str
	}
	if str == "(" {
		cons, err := parseCons(e, t)
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
		m, err := parseMacro(e, tok, t)
		if err != nil {
			return nil, err
		}
		return m, nil
	}
	atom, err1 := ParseAtom(e, tok)
	if err1 != nil {
		return nil, err1
	}
	return atom, nil
}
