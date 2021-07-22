package tokenizer

import (
	"bufio"
	"io"
	"regexp"
	"strings"
)

// Reader interface type is the interface
// for reading string with every token
// Reader is like bufio.Reader but has PeekRune
// which returns a rune without advancing pointer
type Reader struct {
	err error
	ru  rune
	sz  int
	Raw io.Reader
	br  *bufio.Reader
}

// NewReader creates interal reader from io.RuneReader
func NewReader(r io.Reader) *Reader {
	b := new(Reader)
	b.Raw = r
	b.br = bufio.NewReader(r)
	return b
}

// PeekRune returns a rune without advancing pointer
func (r *Reader) PeekRune() (rune, int, error) {
	if r.ru == 0 {
		r.ru, r.sz, r.err = r.br.ReadRune()
	}
	return r.ru, r.sz, r.err
}

// ReadRune returns a rune with advancing pointer
func (r *Reader) ReadRune() (rune, int, error) {
	if r.ru == 0 {
		r.ru, r.sz, r.err = r.br.ReadRune()
	}
	ru := r.ru
	sz := r.sz
	err := r.err
	r.ru, r.sz, r.err = r.br.ReadRune()
	return ru, sz, err
}

func (r *Reader) Read(b []byte) (int, error) {
	ru := r.ru
	sz := r.sz
	err := r.err
	r.ru, r.sz, r.err = r.br.ReadRune()
	copy(b, []byte(string([]rune{ru})))
	return sz, err
}

var str = `^1\+$|^1-$|` +
	`^[-+]?[[:digit:]]+\.[[:digit:]]+$|` +
	`^[-+]?[[:digit:]]+(?:\.[[:digit:]]+)?[eE][-+]?[[:digit:]]+$|` +
	`^[-+]?[[:digit:]]+$|` +
	`^#[bB][-+]?[01]+$|` +
	`^#[oO][-+]?[0-7]+$|` +
	`^#[xX][-+]?[[:xdigit:]]+$|` +
	`^#\\[[:alpha:]]+$|` +
	`^#\\[[:graph:]]$|` +
	`^"(?:\\\\|\\.|[^\\"])*"$|` +
	`^[:&][a-zA-Z]+$|` +
	`^\+$|^-$|^[a-zA-Z<>/*=?_!$%[\]^{}~][-a-zA-Z0-9+<>/*=?_!$%[\]^{}~]*$|` +
	`^\|(?:\\\\|\\.|[^\\|])*\|$|` +
	`^[.()]$|` +
	"^;.*\n$|" +
	`^#\|.*?\|#$|` +
	"^#'$|^,@?$|^'$|^`$|^#[[:digit:]]*[aA]$|^#$" // TODO: hangs at #ab or #3
var re = regexp.MustCompile(str)

// ReadToken returns error or string as token
func (r *Reader) Next() (string, error) {
	for {
		ru, _, err := r.PeekRune()
		if ru == 0 || err != nil {
			return "", io.EOF
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
		ru, _, err := r.PeekRune()
		if ru == 0 || err != nil {
			if mat {
				return buf, nil
			}
			return "", io.EOF
		}
		if buf == "" && strings.ContainsRune("1234567890", ru) {
			num = true
		}
		if buf == "" && '#' == ru {
			shp = true
		}
		if mat && !re.MatchString(buf+string(ru)) {
			if num && strings.ContainsRune(".Ee", ru) {
				buf += string(ru)
				r.ReadRune()
				continue
			}
			if shp {
				if len(buf) == 1 && ru == '\\' {
					buf += string(ru)
					r.ReadRune()
					continue
				}
				if matched, _ := regexp.MatchString(`^#[0123456789]*$`, buf+string(ru)); matched {
					buf += string(ru)
					r.ReadRune()
					continue
				}
			}
			break
		}
		buf += string(ru)
		if re.MatchString(buf) {
			mat = true
		}
		r.ReadRune()
	}
	return buf, nil
}
