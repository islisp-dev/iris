// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package ilos

import (
	"fmt"
)

type List interface {
	Slice() []Instance
	Nth(i int) Instance
	SetNth(obj Instance, i int)
	NthCdr(i int) Instance
	Length() int
}

// Cons

type Cons struct {
	Car Instance
	Cdr Instance
}

func NewCons(car, cdr Instance) Instance {
	return &Cons{car, cdr}
}

func (*Cons) Class() Class {
	return ConsClass
}

func (i *Cons) String() string {
	str := "(" + fmt.Sprint(i.Car)
	cdr := i.Cdr
	for InstanceOf(ConsClass, cdr) {
		str += fmt.Sprintf(" %v", cdr.(*Cons).Car) // Checked at the top of this loop
		cdr = cdr.(*Cons).Cdr                      // Checked at the top of this loop
	}
	if InstanceOf(NullClass, cdr) {
		str += ")"
	} else {
		str += fmt.Sprintf(" . %v)", cdr)
	}
	return str
}

func (i *Cons) Slice() []Instance {
	s := []Instance{}
	var cdr Instance = i
	for InstanceOf(ConsClass, cdr) {
		s = append(s, cdr.(*Cons).Car)
		cdr = cdr.(*Cons).Cdr
	}
	return s
}

func (i *Cons) Length() int {
	return 1 + i.Cdr.(List).Length()
}

func (i *Cons) Nth(n int) Instance {
	if n == 0 {
		return i.Car
	}
	return i.Cdr.(List).Nth(n - 1)
}

func (i *Cons) SetNth(obj Instance, n int) {
	if n == 0 {
		i.Car = obj
	}
	i.Cdr.(List).SetNth(obj, n-1)
}

func (i *Cons) NthCdr(n int) Instance {
	if n == 0 {
		return i.Cdr
	}
	return i.Cdr.(List).NthCdr(n - 1)
}

// Null

type Null struct{}

var Nil = NewNull()

func NewNull() Instance {
	return &Null{}
}

func (*Null) Class() Class {
	return NullClass
}

func (*Null) String() string {
	return "NIL"
}

func (*Null) Slice() []Instance {
	return []Instance{}
}

func (i *Null) Nth(n int) Instance {
	return Nil
}

func (i *Null) SetNth(obj Instance, n int) {
	panic("NOT a cons")
}

func (i *Null) NthCdr(n int) Instance {
	return Nil
}

func (i *Null) Length() int {
	return 0
}
