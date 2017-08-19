// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
)

type List interface {
	Slice() []ilos.Instance
}

//
// Cons
//

type Cons struct {
	Car ilos.Instance
	Cdr ilos.Instance
}

func NewCons(car, cdr ilos.Instance) ilos.Instance {
	return &Cons{car, cdr}
}

func (*Cons) Class() ilos.Class {
	return ConsClass
}

func (i *Cons) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	if symbol, ok := key.(Symbol); ok {
		switch symbol {
		case "CAR":
			return i.Car, true
		case "CDR":
			return i.Cdr, true
		}
	}
	return nil, false
}

func (i *Cons) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	if symbol, ok := key.(Symbol); ok {
		switch symbol {
		case "CAR":
			i.Car = value
			return true
		case "CDR":
			i.Cdr = value
			return true
		}
	}
	return false
}

func (i *Cons) String() string {
	str := "(" + fmt.Sprint(i.Car)
	cdr := i.Cdr
	for ilos.InstanceOf(ConsClass, cdr) {
		str += fmt.Sprintf(" %v", cdr.(*Cons).Car) // Checked at the top of this loop
		cdr = cdr.(*Cons).Cdr                      // Checked at the top of this loop
	}
	if ilos.InstanceOf(NullClass, cdr) {
		str += ")"
	} else {
		str += fmt.Sprintf(" . %v)", cdr)
	}
	return str
}

func (i *Cons) Slice() []ilos.Instance {
	s := []ilos.Instance{}
	var cdr ilos.Instance = i
	for ilos.InstanceOf(ConsClass, cdr) {
		s = append(s, cdr.(*Cons).Car)
		cdr = cdr.(*Cons).Cdr
	}
	return s
}

//
// Null
//

type Null struct{}

var Nil = NewNull()

func NewNull() ilos.Instance {
	return &Null{}
}

func (*Null) Class() ilos.Class {
	return NullClass
}

func (i *Null) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i *Null) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (*Null) String() string {
	return "NIL"
}

func (*Null) Slice() []ilos.Instance {
	return []ilos.Instance{}
}
