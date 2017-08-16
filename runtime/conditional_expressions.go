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
		return nil, instance.NewArityError()
	}
	if len(elseForm) == 0 {
		return Nil, nil
	}
	return Eval(local, global, elseForm[0])
}

// Cond the clauses (test form*) are scanned sequentially
// and in each case the test is evaluated; when a test delivers a non-nil value
// the scanning process stops and all forms associated with the corresponding clause
//are sequentially evaluated and the value of the last one is returned.
// If no test is true, then nil is returned.
// If no form exists for the successful test then the value of this test is returned.
func Cond(local, global *environment.Environment, testFrom ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	for _, tf := range testFrom {
		if err := ensure(class.List, tf); err != nil {
			return nil, err
		}
		s := tf.(instance.List).Slice()
		if len(s) == 0 {
			return nil, instance.NewArityError()
		}
		ret, err := Eval(local, global, s[0])
		if err != nil {
			return nil, err
		}
		if ret == T {
			return Progn(local, global, s[1:]...)
		}
	}
	return Nil, nil
}

// Case special form, called case form, provide a mechanism
// to execute a matching clause from a series of clauses based on the value of a dispatching form keyform.
//
// The clause to be executed is identified by a set of keys. A key can be any object.
// If the keylist of the last clause is t the associated clause is executed if no key matches the keyform.
//
// keyform is a form to be computed at the beginning of execution of the case form.
// If the result of evaluating keyform is equivalent to a key, then the forms, if any,
// in the corresponding clause are evaluated sequentially and the value of the last one
// is returned as value of the whole case form. case determines match equivalence by using eql;
// the value returned by keyform and key. If no form exists for a matching key, the case form evaluates to nil.
// If the value of keyform is different from every key, and there is a default clause, its forms, if any,
// are evaluated sequentially, and the value of the last one is the result of the case form.
func Case(local, global *environment.Environment, key ilos.Instance, pattern ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	key, err := Eval(local, global, key)
	if err != nil {
		return nil, err
	}
	for idx, pat := range pattern {
		if err := ensure(class.List, pat); err != nil {
			return nil, err
		}
		form := pat.(instance.List).Slice()
		if len(form) < 1 {
			return nil, instance.NewArityError()
		}
		if idx == len(pattern)-1 && form[0] == T {
			return Progn(local, global, form[1:]...)
		}
		if err := ensure(class.List, form[0]); err != nil {
			return nil, err
		}
		keys := form[0].(instance.List).Slice()
		for _, k := range keys {
			if k == key {
				return Progn(local, global, form[1:]...)
			}
		}
	}
	return Nil, nil
}

// CaseUsing special form, called case forms, provide a mechanism
// to execute a matching clause from a series of clauses based on the value of a dispatching form keyform.
//
// The clause to be executed is identified by a set of keys. A key can be any object.
// If the keylist of the last clause is t the associated clause is executed if no key matches the keyform.
//
// keyform is a form to be computed at the beginning of execution of the case form.
// If the result of evaluating keyform is equivalent to a key, then the forms, if any,
// in the corresponding clause are evaluated sequentially and the value of the last one
// is returned as value of the whole case form.
// case-using match determines equivalence by using the result of evaluating predform.
// predform must be a boolean or quasi-boolean function that accepts two arguments,
// the value returned by keyform and key. If no form exists for a matching key, the case form evaluates to nil.
// If the value of keyform is different from every key, and there is a default clause, its forms, if any,
// are evaluated sequentially, and the value of the last one is the result of the case form.
func CaseUsing(local, global *environment.Environment, key, pred ilos.Instance, pattern ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	key, err := Eval(local, global, key)
	if err != nil {
		return nil, err
	}
	if err := ensure(class.Function, pred); err != nil {
		return nil, err
	}
	for idx, pat := range pattern {
		if err := ensure(class.List, pat); err != nil {
			return nil, err
		}
		form := pat.(instance.List).Slice()
		if len(form) < 1 {
			return nil, instance.NewArityError()
		}
		if idx == len(pattern)-1 && form[0] == T {
			return Progn(local, global, form[1:]...)
		}
		if err := ensure(class.List, form[0]); err != nil {
			return nil, err
		}
		keys := form[0].(instance.List).Slice()
		for _, k := range keys {
			ret, err := pred.(instance.Function).Apply(local, global, k, key)
			if err != nil {
				return nil, err
			}
			if ret != Nil {
				return Progn(local, global, form[1:]...)
			}
		}
	}
	return Nil, nil
}
