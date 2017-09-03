// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package tokenizer

import (
	"bufio"
	"io"
	"regexp"

	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// Tokenizer interface type is the interface for reading string with every token
type Tokenizer struct {
	sc *bufio.Scanner
}

var re *regexp.Regexp

func Tokenize(r io.Reader) *Tokenizer {
	str := ``
	str += `1\+|1-|`
	str += `[-+]?[[:digit:]]+\.[[:digit:]]+|`
	str += `[-+]?[[:digit:]]+(?:\.[[:digit:]]+)?[eE][-+]?[[:digit:]]+|`
	str += `[-+]?[[:digit:]]+|`
	str += `#[bB][-+]?[01]+|`
	str += `#[oO][-+]?[0-7]+|`
	str += `#[xX][-+]?[[:xdigit:]]+|`
	str += `#\\[[:alpha:]]+|`
	str += `#\\[[:graph:]]|`
	str += `"(?:\\\\|\\"|[^"])*"|`
	str += `[:&][a-zA-Z]+|`
	str += `\+|-|[a-zA-Z<>/*=?_!$%[\]^{}~][-a-zA-Z0-9+<>/*=?_!$%[\]^{}~]*|`
	str += `\|(?:\\\\|\\\||[^|])*\||`
	str += `[.()]|`
	str += ";.*?\n|"
	str += `#\|.*?\|#|`
	str += "#'|,@?|'|`|#[[:digit:]]*[aA]|#" // TODO: hangs at #ab or #3
	re = regexp.MustCompile(str)
	sc := bufio.NewScanner(r)
	sc.Split(splitter)
	return &Tokenizer{sc}
}

func (t *Tokenizer) Next() (string, ilos.Instance) {
	if t.sc.Scan() {
		return t.sc.Text(), nil
	}
	return "", instance.Create(env.NewEnvironment(nil, nil, nil, nil), class.EndOfStream)
}

func splitter(data []byte, atEOF bool) (advance int, token []byte, err error) {
	if atEOF {
		advance = len(data)
		token = data
		err = io.EOF
		return
	}
	if loc := re.FindIndex(data); loc != nil {
		advance = loc[1]
		token = data[loc[0]:loc[1]]
		err = nil
		return
	}
	return
}
