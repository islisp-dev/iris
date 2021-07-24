// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

// Consp returns t if obj is a cons (instance of class cons); otherwise, returns
// nil. obj may be any ISLISP object.
func Consp(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(ilos.ConsClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// Cons builds a cons from two objects, with obj1 as its car (or `left') part
// and with obj2 as its cdr (or `right') part. An error shall be signaled if the
// requested cons cannot be allocated (error-id. cannot-create-cons). Both obj1
// and obj2 may be any ISLISP object.
func Cons(e ilos.Environment, obj1, obj2 ilos.Instance) (ilos.Instance, ilos.Instance) {
	return ilos.NewCons(obj1, obj2), nil
}

// Car returns the left component of the cons. An error shall be signaled if
// cons is not a cons (error-id. domain-error).
func Car(e ilos.Environment, cons ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.ConsClass, cons); err != nil {
		return nil, err
	}
	return cons.(*ilos.Cons).Car, nil // Checked at the top of this function
}

// Cdr returns the right component of the cons. An error shall be signaled if
// cons is not a cons (error-id. domain-error).
func Cdr(e ilos.Environment, cons ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.ConsClass, cons); err != nil {
		return nil, err
	}
	return cons.(*ilos.Cons).Cdr, nil // Checked at the top of this function
}

// SetCar updates the left component of cons with obj. The returned value is obj
// . An error shall be signaled if cons is not a cons (error-id. domain-error).
// obj may be any ISLISP object.
func SetCar(e ilos.Environment, obj, cons ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.ConsClass, cons); err != nil {
		return nil, err
	}
	cons.(*ilos.Cons).Car = obj
	return obj, nil
}

// SetCdr updates the right component of cons with obj. The returned value is
// obj . An error shall be signaled if cons is not a cons (error-id.
// domain-error). obj may be any ISLISP object.
func SetCdr(e ilos.Environment, obj, cons ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.ConsClass, cons); err != nil {
		return nil, err
	}
	cons.(*ilos.Cons).Cdr = obj
	return obj, nil
}
