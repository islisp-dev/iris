// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// Cons
//

type Cons struct {
	car ilos.Instance
	cdr ilos.Instance
}

func (*Cons) Class() ilos.Class {
	return class.Cons
}

func (i *Cons) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	if symbol, ok := key.(Symbol); ok {
		switch symbol {
		case "CAR":
			return i.car, true
		case "CDR":
			return i.cdr, true
		}
	}
	return nil, false
}

func (i *Cons) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	if symbol, ok := key.(Symbol); ok {
		switch symbol {
		case "CAR":
			i.car = value
			return true
		case "CDR":
			i.cdr = value
			return true
		}
	}
	return false
}

func (i *Cons) String() string {
	str := "(" + fmt.Sprint(i.car)
	cdr := i.cdr
	for Of(class.Cons, cdr) {
		str += fmt.Sprintf(" %v", UnsafeCar(cdr)) // Checked at the top of this loop
		cdr = UnsafeCdr(cdr)                      // Checked at the top of this loop
	}
	if Of(class.Null, cdr) {
		str += ")"
	} else {
		str += fmt.Sprintf(" . %v)", cdr)
	}
	return str
}

func UnsafeCar(i ilos.Instance) ilos.Instance {
	return i.(*Cons).car
}

func UnsafeCdr(i ilos.Instance) ilos.Instance {
	return i.(*Cons).cdr
}

func UnsafeSetCar(o, i ilos.Instance) ilos.Instance {
	i.(*Cons).car = o
	return o
}

func UnsafeSetCdr(o, i ilos.Instance) ilos.Instance {
	i.(*Cons).cdr = o
	return o
}

//
// Null
//

type Null struct{}

func (Null) Class() ilos.Class {
	return class.Null
}

func (i Null) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i Null) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (Null) String() string {
	return "nil"
}
