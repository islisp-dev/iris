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
	Raw io.Reader
	*bufio.Reader
}

// NewReader creates interal reader from io.RuneReader
func NewReader(r io.Reader) *Reader {
	return &Reader{r, bufio.NewReader(r)}
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
		bytes, err := r.Peek(1)
		if err != nil {
			return "", io.EOF
		}
		ru := rune(bytes[0])
		if ru == 0 {
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
		bytes, err := r.Peek(1)
		if err != nil {
			if mat {
				return buf, nil
			}
			return "", io.EOF
		}
		ru := rune(bytes[0])
		if ru == 0 {
			if mat {
				return buf, nil
			}
			return "", io.EOF
		}
		if buf == "" && strings.ContainsRune("1234567890", ru) {
			num = true
		}
		if buf == "" && ru == '#' {
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
				if len(buf) == 1 && (ru == 'b' || ru == 'B' || ru == 'o' || ru == 'O' || ru == 'x' || ru == 'X') {
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
