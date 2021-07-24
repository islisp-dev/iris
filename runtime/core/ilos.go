// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"reflect"

	"github.com/google/go-cmp/cmp"
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

func DeepEqual(x, y interface{}) bool {
	return reflect.DeepEqual(x, y) || cmp.Equal(x, y, cmp.AllowUnexported(BuiltInClass{}, StandardClass{}, Function{}))
}

func SubclassOf(super, sub Class) bool {
	var subclassof func(p, c Class) bool
	subclassof = func(p, c Class) bool {
		if DeepEqual(p, c) {
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

func InstanceOf(c Class, i Instance) bool {
	if DeepEqual(c, i.Class()) {
		return true
	}
	return SubclassOf(c, i.Class())
}
