// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ilos

import (
	"reflect"
)

type Class interface {
	Supers() []Class
	Slots() []Instance
	Initform(Instance) (Instance, bool)
	Initarg(Instance) (Instance, bool)
	Class() Class
	GetSlotValue(Instance, Class) (Instance, bool)
	SetSlotValue(Instance, Instance, Class) bool
	String() string
}

type Instance interface {
	Class() Class
	GetSlotValue(Instance, Class) (Instance, bool)
	SetSlotValue(Instance, Instance, Class) bool
	String() string
}

func SubclassOf(super, sub Class) bool {
	is := func(c, p Class) bool {
		var sub func(c, p Class) bool
		sub = func(c, p Class) bool {
			if reflect.DeepEqual(c, p) {
				return true
			}
			for _, d := range c.Supers() {
				if sub(d, p) {
					return true
				}
			}
			return false
		}
		for _, d := range c.Supers() {
			if sub(d, p) {
				return true
			}
		}
		return false
	}
	return is(sub, super)
}

func InstanceOf(p Class, i Instance) bool {
	if reflect.DeepEqual(i.Class(), p) {
		return true
	}
	return SubclassOf(p, i.Class())
}
