// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import "github.com/ta2gch/iris/runtime/ilos"

type BuiltInClass struct {
	supers []ilos.Class
	slots  []ilos.Instance
	name   string
}

func NewBuiltInClass(name string, super ilos.Class, slots ...string) ilos.Class {
	slotNames := []ilos.Instance{}
	for _, slot := range slots {
		slotNames = append(slotNames, NewSymbol(slot))
	}
	if super == nil {
		return BuiltInClass{[]ilos.Class{}, slotNames, name}
	}
	return BuiltInClass{[]ilos.Class{super}, slotNames, name}
}

func (BuiltInClass) Class() ilos.Class {
	return BuiltInClassClass
}

func (p BuiltInClass) Supers() []ilos.Class {
	return p.supers
}

func (p BuiltInClass) Slots() []ilos.Instance {
	return p.slots
}

func (p BuiltInClass) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (p BuiltInClass) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (p BuiltInClass) String() string {
	return p.name
}
