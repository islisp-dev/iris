// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"fmt"
)

type StandardClass struct {
	name      Instance
	supers    []Class
	slots     []Instance
	initforms map[Instance]Instance
	initargs  map[Instance]Instance
	metaclass Class
	abstractp Instance
}

func NewStandardClass(name Instance, supers []Class, slots []Instance, initforms, initargs map[Instance]Instance, metaclass Class, abstractp Instance) Class {
	return StandardClass{name, supers, slots, initforms, initargs, metaclass, abstractp}
}

func (p StandardClass) Supers() []Class {
	return p.supers
}

func (p StandardClass) Slots() []Instance {
	return p.slots
}

func (p StandardClass) Initform(arg Instance) (Instance, bool) {
	v, ok := p.initforms[arg]
	return v, ok
}

func (p StandardClass) Initarg(arg Instance) (Instance, bool) {
	v, ok := p.initargs[arg]
	return v, ok
}

func (p StandardClass) Class() Class {
	return p.metaclass
}

func (p StandardClass) String() string {
	return fmt.Sprint(p.name)
}
