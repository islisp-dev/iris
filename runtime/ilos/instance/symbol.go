// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/asciian/iris/runtime/ilos"
)

// Symbol

type Symbol string

func NewSymbol(s string) ilos.Instance {
	return Symbol(s)
}

func (Symbol) Class() ilos.Class {
	return SymbolClass
}

func (i Symbol) String() string {
	return string(i)
}

var T = NewSymbol("T")
