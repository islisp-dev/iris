// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"fmt"
)

// Integer

type Integer int

func NewInteger(i int) Instance {
	return Integer(i)
}
func (Integer) Class() Class {
	return IntegerClass
}

func (i Integer) String() string {
	return fmt.Sprint(int(i))
}

// Float

type Float float64

func NewFloat(i float64) Instance {
	return Float(i)
}

func (Float) Class() Class {
	return FloatClass
}

func (i Float) String() string {
	return fmt.Sprint(float64(i))
}
