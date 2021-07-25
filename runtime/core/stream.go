// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"bufio"
	"io"
	"strings"

	"github.com/islisp-dev/iris/reader/tokenizer"
)

type BufferedWriter struct {
	Raw io.Writer
	*bufio.Writer
}

func NewBufferedWriter(w io.Writer) *BufferedWriter {
	return &BufferedWriter{w, bufio.NewWriter(w)}
}

type Stream struct {
	Column       *int
	ElementClass Instance
	*tokenizer.BufferedTokenReader
	*BufferedWriter
}

func NewStream(r io.Reader, w io.Writer, e Instance) Instance {
	return Stream{new(int), e, tokenizer.NewBufferedTokenReader(r), NewBufferedWriter(w)}
}

func (Stream) Class() Class {
	return StreamClass
}

func (s Stream) Write(p []byte) (n int, err error) {
	i := strings.LastIndex(string(p), "\n")
	if i < 0 {
		*s.Column += len(p)
	} else {
		*s.Column = len(p[i+1:])
	}
	return s.Writer.Write(p)
}

func (Stream) String() string {
	return "#<STREAM>"
}
