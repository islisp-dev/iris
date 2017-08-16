// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

// Not is the logical “not” (or “¬”). It returns t if obj is nil
// and nil otherwise. obj may be any ISLISP object.
func Not(local, global environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if obj == Nil {
		return T, nil
	}
	return Nil, nil
}

// And is the sequential logical “and” (or “∧”). forms are evaluated
// from left to right until either one of them evaluates to nil or else
// none are left. If one of them evaluates to nil, then nil is returned
// from the and; otherwise, the value of the last evaluated form is returned.
func And(local, global environment.Environment, form ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	for _, f := range form {
		if f == Nil {
			return Nil, nil
		}
	}
	if len(form) == 0 {
		return T, nil
	}
	return form[len(form)-1], nil
}

// Or is the sequential logical "or" (or "∨"). forms are evaluated
// from left to right until either one of them evaluates to a non-nil value
// or else none are left. If one of them evaluates to a non-nil value,
// then this non-nil value is returned, otherwise nil is returned.
func Or(local, global environment.Environment, form ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	for _, f := range form {
		if f != Nil {
			return f, nil
		}
	}
	return Nil, nil
}
