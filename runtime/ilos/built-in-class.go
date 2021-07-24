// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package ilos

import (
	"fmt"
)

type BuiltInClass struct {
	name   Instance
	supers []Class
	slots  []Instance
}

func NewBuiltInClass(name string, super Class, slots ...string) Class {
	slotNames := []Instance{}
	for _, slot := range slots {
		slotNames = append(slotNames, NewSymbol(slot))
	}
	return BuiltInClass{NewSymbol(name), []Class{super}, slotNames}
}

func (p BuiltInClass) Supers() []Class {
	return p.supers
}

func (p BuiltInClass) Slots() []Instance {
	return p.slots
}

func (p BuiltInClass) Initform(arg Instance) (Instance, bool) {
	return nil, false
}

func (p BuiltInClass) Initarg(arg Instance) (Instance, bool) {
	return arg, true
}

func (BuiltInClass) Class() Class {
	return BuiltInClassClass
}

func (p BuiltInClass) String() string {
	return fmt.Sprint(p.name)
}
