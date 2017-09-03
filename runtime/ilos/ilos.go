// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

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
	String() string
}

type Instance interface {
	Class() Class
	String() string
}

func SubclassOf(super, sub Class) bool {
	var subclassof func(p, c Class) bool
	subclassof = func(p, c Class) bool {
		if reflect.DeepEqual(c, p) {
			return true
		}
		for _, d := range c.Supers() {
			if subclassof(p, d) {
				return true
			}
		}
		return false
	}
	for _, d := range sub.Supers() {
		if subclassof(super, d) {
			return true
		}
	}
	return false
}

func InstanceOf(p Class, i Instance) bool {
	if reflect.DeepEqual(i.Class(), p) {
		return true
	}
	return SubclassOf(p, i.Class())
}
