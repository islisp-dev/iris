// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ilos

type Class interface {
	Class() Class
	Supers() []Class
	Slots() []Instance
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
