package tokenizer

import (
	"bufio"
	"io"
	"regexp"
	"strings"

	"github.com/dlclark/regexp2"
)

// BufferedTokenReader interface type is the interface
// for reading string with every token
// BufferedTokenReader is like bufio.BufferedTokenReader but has PeekRune
// which returns a rune without advancing pointer
type BufferedTokenReader struct {
	line, column int
	Raw          io.Reader
	*bufio.Reader
}

// NewBufferedTokenReader creates interal reader from io.RuneReader
func NewBufferedTokenReader(r io.Reader) *BufferedTokenReader {
	return &BufferedTokenReader{1, 0, r, bufio.NewReader(r)}
}

func (t *BufferedTokenReader) ReadRune() (r rune, size int, err error) {
	r, size, err = t.Reader.ReadRune()
	if r == '\n' {
		t.line++
		t.column = 0
	} else {
		t.column++
	}
	return
}

var str = `^1\+$|^1-$|` +
	`^[-+]?[[:digit:]]+\.[[:digit:]]+$|` +
	`^[-+]?[[:digit:]]+(?:\.[[:digit:]]+)?[eE][-+]?[[:digit:]]+$|` +
	`^[-+]?[[:digit:]]+$|` +
	`^#[bB][-+]?[01]*$|` +
	`^#[oO][-+]?[0-7]*$|` +
	`^#[xX][-+]?[[:xdigit:]]*$|` +
	`^#\\[[:alpha:]]*$|` +
	`^#\\[[:graph:]]?$|` +
	`^"(?:\\\\|\\.|[^\\"])*"$|` +
	`^[:&][a-zA-Z]+$|` +
	`^\+$|^-$|^[a-zA-Z<>/*=?_!$%[\]^{}~][-a-zA-Z0-9+<>/*=?_!$%[\]^{}~]*$|` +
	`^\|(?:\\\\|\\.|[^\\|])*\|$|` +
	`^[.()]$|` +
	"^;[^\n]*$|" +
	`^#\|((?<!\|#)[\s\S])*$|` +
	"^#'$|^,@?$|^'$|^`$|^#[[:digit:]]*[aA]$|^#$" // TODO: hangs at #ab or #3
var re = regexp2.MustCompile(str, regexp2.RE2)

type Token struct {
	Str          string
	Line, Column int
}

func NewToken(Str string, Line, Column int) *Token {
	return &Token{Str, Line, Column}
}

// ReadToken returns error or string as token
func (r *BufferedTokenReader) ReadToken() (*Token, error) {
	for {
		bytes, err := r.Peek(1)
		if err != nil {
			return NewToken("", r.line, r.column), io.EOF
		}
		ru := rune(bytes[0])
		if ru == 0 {
			return NewToken("", r.line, r.column), io.EOF
		}
		if !strings.ContainsRune(" \t\n\r", ru) {
			break
		}
		r.ReadRune()
	}
	buf := ""
	mat := false
	num := false
	shp := false
	for {
		bytes, err := r.Peek(1)
		if err != nil {
			if mat {
				return NewToken(buf, r.line, r.column-len(buf)+1), nil
			}
			return NewToken("", r.line, r.column), nil
		}
		ru := rune(bytes[0])
		if ru == 0 {
			if mat {
				return NewToken(buf, r.line, r.column-len(buf)+1), nil
			}
			return NewToken("", r.line, r.column), nil
		}
		if buf == "" && strings.ContainsRune("1234567890", ru) {
			num = true
		}
		if buf == "" && ru == '#' {
			shp = true
		}
		if m, _ := re.MatchString(buf + string(ru)); !m && mat {
			if num && strings.ContainsRune(".Ee", ru) {
				buf += string(ru)
				r.ReadRune()
				continue
			}
			if shp {
				if matched, _ := regexp.MatchString(`^#[0123456789]*$`, buf+string(ru)); matched {
					buf += string(ru)
					r.ReadRune()
					continue
				}
			}
			break
		}
		buf += string(ru)
		if m, _ := re.MatchString(buf); m {
			mat = true
		}
		r.ReadRune()
	}
	return NewToken(buf, r.line, r.column-len(buf)+1), nil
}
