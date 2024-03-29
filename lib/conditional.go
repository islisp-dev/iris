// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/core"

// If is conditional expression. The test-form is evaluated. If its result is
// anything non-nil, the then-form is evaluated and its value is returned;
// otherwise (if the test-form returned nil), the else-form is evaluated and its
// value is returned. If no else-form is provided, it defaults to nil.
func If(e core.Environment, testForm, thenForm core.Instance, elseForm ...core.Instance) (core.Instance, core.Instance) {
	tf, err := Eval(e, testForm)
	if err != nil {
		return nil, err
	}
	if tf != Nil {
		return Eval(e, thenForm)
	}
	if len(elseForm) > 1 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	if len(elseForm) == 0 {
		return Nil, nil
	}
	return Eval(e, elseForm[0])
}

// Cond the clauses (test form*) are scanned sequentially and in each case the
// test is evaluated; when a test delivers a non-nil value the scanning process
// stops and all forms associated with the corresponding clauseare sequentially
// evaluated and the value of the last one is returned. If no test is true, then
// nil is returned. If no form exists for the successful test then the value of
// this test is returned.
func Cond(e core.Environment, testFrom ...core.Instance) (core.Instance, core.Instance) {
	for _, tf := range testFrom {
		if err := ensure(e, core.ListClass, tf); err != nil {
			return nil, err
		}
		s := tf.(core.List).Slice()
		if len(s) == 0 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		ret, err := Eval(e, s[0])
		if err != nil {
			return nil, err
		}
		if core.DeepEqual(ret, T) {
			return Progn(e, s[1:]...)
		}
	}
	return Nil, nil
}

// Case special form, called case form, provide a mechanism to execute a
// matching clause from a series of clauses based on the value of a dispatching
// form keyform. The clause to be executed is identified by a set of keys. A key
// can be any object. If the keylist of the last clause is t the associated
// clause is executed if no key matches the keyform. keyform is a form to be
// computed at the beginning of execution of the case form. If the result of
// evaluating keyform is equivalent to a key, then the forms, if any, in the
// corresponding clause are evaluated sequentially and the value of the last one
// is returned as value of the whole case form. case determines match
// equivalence by using eql; the value returned by keyform and key. If no form
// exists for a matching key, the case form evaluates to nil. If the value of
// keyform is different from every key, and there is a default clause, its
// forms, if any, are evaluated sequentially, and the value of the last one is
// the result of the case form.
func Case(e core.Environment, key core.Instance, pattern ...core.Instance) (core.Instance, core.Instance) {
	key, err := Eval(e, key)
	if err != nil {
		return nil, err
	}
	for idx, pat := range pattern {
		if err := ensure(e, core.ListClass, pat); err != nil {
			return nil, err
		}
		form := pat.(core.List).Slice()
		if len(form) < 1 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		if idx == len(pattern)-1 && core.DeepEqual(form[0], T) {
			return Progn(e, form[1:]...)
		}
		if err := ensure(e, core.ListClass, form[0]); err != nil {
			return nil, err
		}
		for _, k := range form[0].(core.List).Slice() {
			if core.DeepEqual(k, key) {
				return Progn(e, form[1:]...)
			}
		}
	}
	return Nil, nil
}

// CaseUsing special form, called case forms, provide a mechanism to execute a
// matching clause from a series of clauses based on the value of a dispatching
// form keyform. The clause to be executed is identified by a set of keys. A key
// can be any object. If the keylist of the last clause is t the associated
// clause is executed if no key matches the keyform. keyform is a form to be
// computed at the beginning of execution of the case form. If the result of
// evaluating keyform is equivalent to a key, then the forms, if any, in the
// corresponding clause are evaluated sequentially and the value of the last one
// is returned as value of the whole case form. case-using match determines
// equivalence by using the result of evaluating predform. predform must be a
// boolean or quasi-boolean function that accepts two arguments, the value
// returned by keyform and key. If no form exists for a matching key, the case
// form evaluates to nil. If the value of keyform is different from every key,
// and there is a default clause, its forms, if any, are evaluated sequentially,
// and the value of the last one is the result of the case form.
func CaseUsing(e core.Environment, pred, key core.Instance, pattern ...core.Instance) (core.Instance, core.Instance) {
	key, err := Eval(e, key)
	if err != nil {
		return nil, err
	}
	pred, err = Eval(e, pred)
	if err != nil {
		return nil, err
	}
	if err := ensure(e, core.FunctionClass, pred); err != nil {
		return nil, err
	}
	for idx, pat := range pattern {
		if err := ensure(e, core.ListClass, pat); err != nil {
			return nil, err
		}
		form := pat.(core.List).Slice()
		if len(form) < 1 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		if idx == len(pattern)-1 && core.DeepEqual(form[0], T) {
			return Progn(e, form[1:]...)
		}
		if err := ensure(e, core.ListClass, form[0]); err != nil {
			return nil, err
		}
		for _, k := range form[0].(core.List).Slice() {
			ret, err := pred.(core.Applicable).Apply(e.NewDynamic(), k, key)
			if err != nil {
				return nil, err
			}
			if ret != Nil {
				return Progn(e, form[1:]...)
			}
		}
	}
	return Nil, nil
}
