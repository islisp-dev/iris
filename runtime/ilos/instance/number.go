// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
)

//
// Integer
//

type Integer int

func NewInteger(i int) ilos.Instance {
	return Integer(i)
}
func (Integer) Class() ilos.Class {
	return IntegerClass
}

func (i Integer) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i Integer) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (i Integer) String() string {
	return fmt.Sprint(int(i))
}

//
// Float
//

type Float float64

func NewFloat(i float64) ilos.Instance {
	return Float(i)
}

func (Float) Class() ilos.Class {
	return FloatClass
}

func (i Float) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i Float) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (i Float) String() string {
	return fmt.Sprint(float64(i))
}
