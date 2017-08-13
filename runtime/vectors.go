// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// BasicVectorP returns t if obj is a basic-vector (instance of class basic-vector);
// otherwise, returns nil. obj may be any ISLISP object.
func BasicVectorP(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.BasicVector, obj) {
		return T, nil
	}
	return Nil, nil
}

// GeneralVectorP returns t if obj is a general-vector (instance of class general-vector);
// otherwise, returns nil. obj may be any ISLISP object.
func GeneralVectorP(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.GeneralVector, obj) {
		return T, nil
	}
	return Nil, nil
}

// CreateVector returns a general-vector of length i. If initial-element is given,
// the elements of the new vector are initialized with this object,
// otherwise the initialization is implementation defined. An error shall be signaled
// if the requested vector cannot be allocated (error-id. cannot-create-vector).
// An error shall be signaled if i is not a non-negative integer (error-id. domain-error).
// initial-element may be any ISLISP object.
func CreateVector(_, _ *environment.Environment, i ilos.Instance, initialElement ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if !instance.Of(class.Integer, i) || int(i.(instance.Integer)) < 0 {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         i,
			"EXPECTED-CLASS": class.Integer,
		})
	}
	if len(initialElement) > 1 {
		return nil, instance.New(class.ProgramError)
	}
	n := int(i.(instance.Integer))
	v := make([]ilos.Instance, n)
	for i := 0; i < n; i++ {
		if len(initialElement) == 0 {
			v[i] = Nil
		} else {
			v[i] = initialElement[0]
		}
	}
	return instance.New(class.GeneralVector, v), nil
}

// Vector returns a new general-vector whose elements are its obj arguments.
// The length of the newly created vector is, therefore, the number of objs passed as arguments.
// The vector is indexed by integers ranging from 0 to dimension−1. An error shall be signaled
// if the requested vector cannot be allocated (error-id. cannot-create-vector).
// Each obj may be any ISLISP object.
func Vector(_, _ *environment.Environment, obj ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	return instance.New(class.GeneralVector, obj), nil
}
