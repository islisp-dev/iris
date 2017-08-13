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

// TODO: if, cond, case, case-using

// If is conditional expression.
// The test-form is evaluated. If its result is anything non-nil,
// the then-form is evaluated and its value is returned;
// otherwise (if the test-form returned nil), the else-form
// is evaluated and its value is returned.
//
// If no else-form is provided, it defaults to nil.
func If(local, global *environment.Environment, testForm, thenForm ilos.Instance, elseForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	tf, err := Eval(local, global, testForm)
	if err != nil {
		return nil, err
	}
	if tf == T {
		return Eval(local, global, thenForm)
	}
	if len(elseForm) > 1 {
		return nil, instance.New(class.ProgramError)
	}
	if len(elseForm) == 0 {
		return Nil, nil
	}
	return Eval(local, global, elseForm[0])
}

func Cond(local, global *environment.Environment, testFrom ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	for _, tf := range testFrom {
		s, ln, err := convSlice(tf)
		if err != nil {
			return nil, err
		}
		if ln == 0 {
			return nil, instance.New(class.ProgramError)
		}
		ret, err := Eval(local, global, s[0])
		if err != nil {
			return nil, err
		}
		if ret == T {
			var err ilos.Instance
			ret := Nil
			for _, e := range s[1:] {
				ret, err = Eval(local, global, e)
				if err != nil {
					return nil, err
				}
			}
			return ret, nil
		}
	}
	return Nil, nil
}
