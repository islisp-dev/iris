// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// While the test-form returns a true value. Specifically:
//
// 1. test-form is evaluated, producing a value Vt.
//
// 2. If Vt is nil, then the while form immediately returns nil.
//
// 3. Otherwise, if Vt is non-nil, the forms body-form* are evaluated sequentially (from left to right).
//
// 4. Upon successful completion of the body-forms*, the while form begins again with step 1.
func While(e env.Environment, testForm ilos.Instance, bodyForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	test, err := Eval(e, testForm)
	if err != nil {
		return nil, err
	}
	for test == T {
		_, err := Progn(e, bodyForm...)
		if err != nil {
			return nil, err
		}
		test, err = Eval(e, testForm)
		if err != nil {
			return nil, err
		}
	}
	return Nil, nil
}

// For repeatedly executes a sequence of forms form*, called its body. It specifies a set of identifiers naming
// variables that will be e to the for form, their initialization, and their update for each iteration.
// When a termination condition is met, the iteration exits with a specified result value.
//
// The scope of an identifier var is the body, the steps, the end-test , and the result *. A step might be omitted,
// in which case the effect is the same as if (var init var) had been written instead of (var init).
// It is a violation if more than one iteration-spec names the same var in the same for form.
//
// The for special form is executed as follows: The init forms are evaluated sequentially from left to right.
// Then each value is used as the initial value of the variable denoted by the corresponding identifier var ,
// and the iteration phase begins.
//
// Each iteration begins by evaluating end-test . If the result is nil, the forms in the body are
// evaluated sequentially (for side-effects). Afterwards, the step-forms are evaluated sequentially
// order from left to right. Then their values are assigned to the corresponding variables and the next iteration begins.
// If end-test returns a non-nil value, then the result * are evaluated sequentially and the value of the
// last one is returned as value of the whole for macro. If no result is present, then the value of the for macro is nil.
func For(e env.Environment, iterationSpecs, endTestAndResults ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.List, iterationSpecs); err != nil {
		return nil, err
	}
	a := e.NewLexical()
	for _, is := range iterationSpecs.(instance.List).Slice() {
		if err := ensure(e, class.List, is); err != nil {
			return nil, err
		}
		i := is.(instance.List).Slice()
		switch len(i) {
		case 2, 3:
			var1 := i[0]
			init, err := Eval(e, i[1])
			if err != nil {
				return nil, err
			}
			if !a.Variable.Define(var1, init) {
				return SignalCondition(e, instance.NewImmutableBinding(e), Nil)
			}
		default:
		return SignalCondition(e, instance.NewArityError(e), Nil)
		}
	}
	if err := ensure(e, class.List, endTestAndResults); err != nil {
		return nil, err
	}
	ends := endTestAndResults.(instance.List).Slice()
	if len(ends) == 0 {
		return SignalCondition(e, instance.NewArityError(e), Nil)
	}
	endTest := ends[0]
	results := ends[1:]
	test, err := Eval(a, endTest)
	if err != nil {
		return nil, err
	}
	for test == Nil {
		_, err := Progn(a, forms...)
		if err != nil {
			return nil, err
		}
		b := a.NewLexical()
		for _, is := range iterationSpecs.(instance.List).Slice() {
			if err := ensure(e, class.List, is); err != nil {
				return nil, err
			}
			switch is.(instance.List).Length() {
			case 2:
			case 3:
				var1 := is.(instance.List).Nth(0)
				step, err := Eval(a, is.(instance.List).Nth(2))
				if err != nil {
					return nil, err
				}
				if !b.Variable.Define(var1, step) {
					return SignalCondition(e, instance.NewImmutableBinding(e), Nil)
				}
			default:
		return SignalCondition(e, instance.NewArityError(e), Nil)
			}
		}
		test, err = Eval(b, endTest)
		if err != nil {
			return nil, err
		}
		a = b
	}
	return Progn(a, results...)
}
