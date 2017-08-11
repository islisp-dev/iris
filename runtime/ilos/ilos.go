// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ilos

type Class interface {
	Class() Class
	Parents() []Class
	Slots() []string
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

func ChildOf(c, p Class) bool {
	var sub func(c, p Class) bool
	sub = func(c, p Class) bool {
		if c == p {
			return true
		}
		for _, d := range c.Parents() {
			if sub(d, p) {
				return true
			}
		}
		return false
	}
	for _, d := range c.Parents() {
		if sub(d, p) {
			return true
		}
	}
	return false
}

func InstanceOf(i Instance, p Class) bool {
	if i.Class() == p {
		return true
	}
	return ChildOf(i.Class(), p)
}
