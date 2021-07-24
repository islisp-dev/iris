// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/runtime/core"

// Consp returns t if obj is a cons (instance of class cons); otherwise, returns
// nil. obj may be any ISLISP object.
func Consp(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.ConsClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// Cons builds a cons from two objects, with obj1 as its car (or `left') part
// and with obj2 as its cdr (or `right') part. An error shall be signaled if the
// requested cons cannot be allocated (error-id. cannot-create-cons). Both obj1
// and obj2 may be any ISLISP object.
func Cons(e core.Environment, obj1, obj2 core.Instance) (core.Instance, core.Instance) {
	return core.NewCons(obj1, obj2), nil
}

// Car returns the left component of the cons. An error shall be signaled if
// cons is not a cons (error-id. domain-error).
func Car(e core.Environment, cons core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.ConsClass, cons); err != nil {
		return nil, err
	}
	return cons.(*core.Cons).Car, nil // Checked at the top of this function
}

// Cdr returns the right component of the cons. An error shall be signaled if
// cons is not a cons (error-id. domain-error).
func Cdr(e core.Environment, cons core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.ConsClass, cons); err != nil {
		return nil, err
	}
	return cons.(*core.Cons).Cdr, nil // Checked at the top of this function
}

// SetCar updates the left component of cons with obj. The returned value is obj
// . An error shall be signaled if cons is not a cons (error-id. domain-error).
// obj may be any ISLISP object.
func SetCar(e core.Environment, obj, cons core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.ConsClass, cons); err != nil {
		return nil, err
	}
	cons.(*core.Cons).Car = obj
	return obj, nil
}

// SetCdr updates the right component of cons with obj. The returned value is
// obj . An error shall be signaled if cons is not a cons (error-id.
// domain-error). obj may be any ISLISP object.
func SetCdr(e core.Environment, obj, cons core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.ConsClass, cons); err != nil {
		return nil, err
	}
	cons.(*core.Cons).Cdr = obj
	return obj, nil
}
