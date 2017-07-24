package main

import (
	"regexp"
	"strings"
)

type TokenReader interface {
	ReadToken() (rune, int, error)
}

func NewMatcher(src ...string) *regexp.Regexp {
	return regexp.MustCompile("^(?:" + strings.Join(src, ")$|^(?:") + ")$")
}

var MacroToken = ",@|,|'|`"
var IntegerToken = "[+-]?[[:digit:]]+|#[bB][01]+|#[oO][0-7]+|#[xX][[:xdigit:]]+"
var FloatToken = "[+-]?[[:digit:]]+(?:.[[:digit:]]+|[eE][+-]?[[:digit:]]+|.[[:digit:]]+[eE][+-]?[[:digit:]]+)"
var CharacterToken = "#\\\\?|#\\\\(?:[[:alpha:]]+|[[:graph:]])"
var StringToken = "\"(?:\\\\\"|[^\"])*\"?"
var SymbolToken = "[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*|\\|(?:\\\\\\||[^|])*\\|?"
var ParenthesesToken = "\\(|\\)"

var token = NewMatcher(
	MacroToken,
	IntegerToken,
	FloatToken,
	CharacterToken,
	StringToken,
	SymbolToken,
	ParenthesesToken)

func (tr *Reader) ReadToken() (string, error) {
	buf := ""
	for {
		if buf == "" {
			p, _, err := tr.PeekRune()
			if err != nil {
				return "", err
			}
			if token.MatchString(string(p)) {
				buf = string(p)
			}
		} else {
			p, _, err := tr.PeekRune()
			if err != nil {
				return buf, nil
			}
			if !token.MatchString(buf + string(p)) {
				return buf, nil
			}
			buf += string(p)
		}
		tr.ReadRune()
	}
}
