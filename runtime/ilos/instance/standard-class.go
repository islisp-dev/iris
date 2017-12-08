// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"

	"github.com/asciian/iris/runtime/ilos"
)

type StandardClass struct {
	name      ilos.Instance
	supers    []ilos.Class
	slots     []ilos.Instance
	initforms map[ilos.Instance]ilos.Instance
	initargs  map[ilos.Instance]ilos.Instance
	metaclass ilos.Class
	abstractp ilos.Instance
}

func NewStandardClass(name ilos.Instance, supers []ilos.Class, slots []ilos.Instance, initforms, initargs map[ilos.Instance]ilos.Instance, metaclass ilos.Class, abstractp ilos.Instance) ilos.Class {
	return StandardClass{name, supers, slots, initforms, initargs, metaclass, abstractp}
}

func (p StandardClass) Supers() []ilos.Class {
	return p.supers
}

func (p StandardClass) Slots() []ilos.Instance {
	return p.slots
}

func (p StandardClass) Initform(arg ilos.Instance) (ilos.Instance, bool) {
	v, ok := p.initforms[arg]
	return v, ok
}

func (p StandardClass) Initarg(arg ilos.Instance) (ilos.Instance, bool) {
	v, ok := p.initargs[arg]
	return v, ok
}

func (p StandardClass) Class() ilos.Class {
	return p.metaclass
}

func (p StandardClass) String() string {
	return fmt.Sprint(p.name)
}
