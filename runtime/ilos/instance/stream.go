// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"io"

	"github.com/ta2gch/iris/runtime/ilos"
)

type Stream struct {
	Reader io.Reader
	Writer io.Writer
}

func NewStream(r io.Reader, w io.Writer) ilos.Instance {
	return Stream{r, w}
}

func (Stream) Class() ilos.Class {
	return StreamClass
}

func (Stream) String() string {
	return "#<INPUT-STREAM>"
}
