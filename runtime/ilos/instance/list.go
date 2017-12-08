// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"

	"github.com/asciian/iris/runtime/ilos"
)

type List interface {
	Slice() []ilos.Instance
	Nth(i int) ilos.Instance
	SetNth(obj ilos.Instance, i int)
	NthCdr(i int) ilos.Instance
	Length() int
}

// Cons

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

func (i *Cons) Length() int {
	return 1 + i.Cdr.(List).Length()
}

func (i *Cons) Nth(n int) ilos.Instance {
	if n == 0 {
		return i.Car
	}
	return i.Cdr.(List).Nth(n - 1)
}

func (i *Cons) SetNth(obj ilos.Instance, n int) {
	if n == 0 {
		i.Car = obj
	}
	i.Cdr.(List).SetNth(obj, n-1)
}

func (i *Cons) NthCdr(n int) ilos.Instance {
	if n == 0 {
		return i.Cdr
	}
	return i.Cdr.(List).NthCdr(n - 1)
}

// Null

type Null struct{}

var Nil = NewNull()

func NewNull() ilos.Instance {
	return &Null{}
}

func (*Null) Class() ilos.Class {
	return NullClass
}

func (*Null) String() string {
	return "NIL"
}

func (*Null) Slice() []ilos.Instance {
	return []ilos.Instance{}
}

func (i *Null) Nth(n int) ilos.Instance {
	return Nil
}

func (i *Null) SetNth(obj ilos.Instance, n int) {
	panic("NOT a cons")
}

func (i *Null) NthCdr(n int) ilos.Instance {
	return Nil
}

func (i *Null) Length() int {
	return 0
}
