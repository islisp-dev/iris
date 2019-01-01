// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
)

// Not is the logical “not” (or “¬”). It returns t if obj is nil and nil
// otherwise. obj may be any ISLISP object.
func Not(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if obj == Nil {
		return T, nil
	}
	return Nil, nil
}

// And is the sequential logical “and” (or “∧”). forms are evaluated from left
// to right until either one of them evaluates to nil or else none are left. If
// one of them evaluates to nil, then nil is returned from the and; otherwise,
// the value of the last evaluated form is returned.
func And(e env.Environment, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var ret ilos.Instance
	for _, form := range forms {
		//fmt.Printf("%v\n%#v\n", form, e.Variable)
		var err ilos.Instance
		ret, err = Eval(e, form)
		if err != nil {
			return nil, err
		}
		if ret == Nil {
			return Nil, nil
		}
	}
	if len(forms) == 0 {
		return T, nil
	}
	return ret, nil
}

// Or is the sequential logical "or" (or "∨"). forms are evaluated from left to
// right until either one of them evaluates to a non-nil value or else none are
// left. If one of them evaluates to a non-nil value, then this non-nil value is
// returned, otherwise nil is returned.
func Or(e env.Environment, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var ret ilos.Instance
	for _, form := range forms {
		var err ilos.Instance
		ret, err = Eval(e, form)
		if err != nil {
			return nil, err
		}
		if ret != Nil {
			return ret, nil
		}
	}
	return Nil, nil
}
