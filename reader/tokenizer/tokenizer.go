package tokenizer

import (
	"bufio"
	"io"
	"regexp"

	"github.com/ta2gch/iris/runtime/class"
	"github.com/ta2gch/iris/runtime/class/parseerror"
)

// Tokenizer interface type is the interface
// for reading string with every token

type Tokenizer struct {
	sc *bufio.Scanner
}

var re *regexp.Regexp

func New(r io.Reader) *Tokenizer {
	str := ""
	str += "[-+]?[[:digit:]]+|"
	str += "#[bB][-+]?[01]+|"
	str += "#[oO][-+]?[0-7]+|"
	str += "#[xX][-+]?[[:xdigit:]]+|"
	str += "[-+]?[[:digit:]]+\\.[[:digit:]]+|"
	str += "[-+]?[[:digit:]]+(?:\\.[[:digit:]]+)?[eE][-+]?[[:digit:]]+|"
	str += "#\\\\newline|"
	str += "#\\\\space|"
	str += "#\\\\[[:graph:]]|"
	str += "\".*\"|"
	str += ":[<>/*=?_!$%[\\]^{}~0-9a-zA-Z]+|"
	str += "\\|.*\\||"
	str += "[<>/*=?_!$%[\\]^{}~a-zA-Z][<>/*=?_!$%[\\]^{}~0-9a-zA-Z]*|"
	str += "[.()]|"
	str += ",@?|'|`|#[[:digit:]]*[aA]"
	re = regexp.MustCompile(str)
	sc := bufio.NewScanner(r)
	sc.Split(splitter)
	return &Tokenizer{sc}
}

func (t *Tokenizer) Next() (string, class.Instance) {
	if t.sc.Scan() {
		return t.sc.Text(), nil
	}
	return "", parseerror.New("", class.Object)
}

func splitter(data []byte, atEOF bool) (advance int, token []byte, err error) {
	if atEOF {
		return len(data), data, nil
	}
	if loc := re.FindIndex(data); loc != nil {
		return loc[1], data[loc[0]:loc[1]], nil
	}
	return
}
