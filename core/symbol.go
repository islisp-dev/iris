// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import hashstructure "github.com/mitchellh/hashstructure/v2"

// Symbol

type Symbol struct {
	str          string
	line, column int
}

func (x Symbol) Hash() (uint64, error) {
	return hashstructure.Hash(x.str, hashstructure.FormatV2, nil)
}

func (x Symbol) Location() (line, column int) {
	return x.line, x.column
}

func (x Symbol) Equal(y Instance) bool {
	z, ok := y.(Symbol)
	if !ok {
		return false
	}
	return x.str == z.str
}

func NewSymbol(s string, pos ...int) Instance {
	if len(pos) != 2 {
		return Symbol{s, -1, -1}
	}
	return Symbol{s, pos[0], pos[1]}
}

func (Symbol) Class() Class {
	return SymbolClass
}

func (i Symbol) String() string {
	return i.str
}

var T = NewSymbol("T")
