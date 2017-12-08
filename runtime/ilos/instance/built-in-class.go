// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"

	"github.com/asciian/iris/runtime/ilos"
)

type BuiltInClass struct {
	name   ilos.Instance
	supers []ilos.Class
	slots  []ilos.Instance
}

func NewBuiltInClass(name string, super ilos.Class, slots ...string) ilos.Class {
	slotNames := []ilos.Instance{}
	for _, slot := range slots {
		slotNames = append(slotNames, NewSymbol(slot))
	}
	return BuiltInClass{NewSymbol(name), []ilos.Class{super}, slotNames}
}

func (p BuiltInClass) Supers() []ilos.Class {
	return p.supers
}

func (p BuiltInClass) Slots() []ilos.Instance {
	return p.slots
}

func (p BuiltInClass) Initform(arg ilos.Instance) (ilos.Instance, bool) {
	return nil, false
}

func (p BuiltInClass) Initarg(arg ilos.Instance) (ilos.Instance, bool) {
	return arg, true
}

func (BuiltInClass) Class() ilos.Class {
	return BuiltInClassClass
}

func (p BuiltInClass) String() string {
	return fmt.Sprint(p.name)
}
