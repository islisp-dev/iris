// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/core"

// BasicVectorP returns t if obj is a basic-vector (instance of class
// basic-vector); otherwise, returns nil. obj may be any ISLISP object.
func BasicVectorP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.BasicVectorClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// GeneralVectorP returns t if obj is a general-vector (instance of class
// general-vector); otherwise, returns nil. obj may be any ISLISP object.
func GeneralVectorP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.GeneralVectorClass, obj) {
		return T, nil
	}
	return Nil, nil
}

// CreateVector returns a general-vector of length i. If initial-element is
// given, the elements of the new vector are initialized with this object,
// otherwise the initialization is implementation defined. An error shall be
// signaled if the requested vector cannot be allocated (error-id.
// cannot-create-vector). An error shall be signaled if i is not a non-negative
// integer (error-id. domain-error). initial-element may be any ISLISP object.
func CreateVector(e core.Environment, i core.Instance, initialElement ...core.Instance) (core.Instance, core.Instance) {
	if !core.InstanceOf(core.IntegerClass, i) || int(i.(core.Integer)) < 0 {
		return SignalCondition(e, core.NewDomainError(e, i, core.IntegerClass), Nil)
	}
	if len(initialElement) > 1 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	n := int(i.(core.Integer))
	v := make([]core.Instance, n)
	for i := 0; i < n; i++ {
		if len(initialElement) == 0 {
			v[i] = Nil
		} else {
			v[i] = initialElement[0]
		}
	}
	return core.GeneralVector(v), nil
}

// Vector returns a new general-vector whose elements are its obj arguments. The
// length of the newly created vector is, therefore, the number of objs passed
// as arguments. The vector is indexed by integers ranging from 0 to
// dimension−1. An error shall be signaled if the requested vector cannot be
// allocated (error-id. cannot-create-vector). Each obj may be any ISLISP object.
func Vector(e core.Environment, obj ...core.Instance) (core.Instance, core.Instance) {
	return core.GeneralVector(obj), nil
}
