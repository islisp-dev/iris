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

// Setq represents an assignment to the variable denoted by the identifier. In consequence,
// the identifier may designate a different object than before, the value of form.
//
// The result of the evaluation of form is returned. This result is used to
// modify the variable binding denoted by the identifier var (if it is mutable).
// setq can be used only for modifying bindings, and not for establishing a variable.
// The setq special form must be contained in the scope of var , established by defglobal,
// let, let*, for, or a lambda expression.
func Setq(local, global *environment.Environment, var1, form ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := Eval(local, global, form)
	if err != nil {
		return nil, err
	}
	if local.Variable.Set(var1, ret) {
		return ret, nil
	}
	if global.Variable.Set(var1, ret) {
		return ret, nil
	}
	return nil, instance.New(class.UndefinedVariable, map[string]ilos.Instance{
		"NAME":      var1,
		"NAMESPACE": instance.New(class.Symbol, "VARIABLE"),
	})
}

// TODO: Setf

// Let is used to define a scope for a group of identifiers
// for a sequence of forms body-form* (collectively referred to as the body).
// The list of pairs (var form)* is called the let variable list.
// The scope of the identifier var is the body.
//
// The forms are evaluated sequentially from left to right;
// then each variable denoted by the identifier var is initialized to the corresponding value.
// Using these bindings along with the already existing bindings of visible
// identifiers the body-forms are evaluated. The returned value of let is the result
// of the evaluation of the last body-form of its body (or nil if there is none).
//
// No var may appear more than once in let variable list.
func Let(local, global *environment.Environment, varForm ilos.Instance, bodyForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	cdr := varForm
	vfs := map[ilos.Instance]ilos.Instance{}
	for instance.Of(class.Cons, cdr) {
		uCar, uCdr := instance.UnsafeCar, instance.UnsafeCdr
		cadr, cddr := uCar(cdr), uCdr(cdr) // Checked at the top of this loop
		if !(instance.Of(class.Cons, cadr) && instance.Of(class.Cons, uCdr(cadr)) && uCdr(uCdr(cadr)) == Nil) {
			return nil, instance.New(class.ProgramError)
		}
		v := uCar(cadr) // Checked before statement
		f, err := Eval(local, global, uCar(uCdr(cadr)))
		if err != nil {
			return nil, err
		}
		vfs[v] = f
		cdr = cddr
	}
	for v, f := range vfs {
		if local.Variable.Define(v, f) {
			return nil, instance.New(class.ProgramError)
		}
	}
	var err ilos.Instance
	ret := Nil
	for _, form := range bodyForm {
		ret, err = Eval(local, global, form)
		if err != nil {
			return nil, err
		}
	}
	return ret, nil
}

// LetStar form is used to define a scope for a group of identifiers for a sequence
// of forms body-form* (collectively referred to as the body).
// The first subform (the let* variable list) is a list of pairs (var form).
// The scope of an identifier var is the body along with all form
// forms following the pair (var form) in the let* variable list.
//
// For each pair (var form) the following is done: form is evaluated in the context
// of the bindings in effect at that point in the evaluation. The result of
// the evaluation is bound to its associated variable named by the identifier var .
// These variable bindings enlarge the set of current valid identifiers perhaps
// shadowing previous variable bindings (in case some var was defined outside),
// and in this enlarged or modified environment the body-forms are executed.
// The returned value of let* is the result of the evaluation of the last form
// of its body (or nil if there is none).
func LetStar(local, global *environment.Environment, varForm ilos.Instance, bodyForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	cdr := varForm
	for instance.Of(class.Cons, cdr) {
		uCar, uCdr := instance.UnsafeCar, instance.UnsafeCdr
		cadr, cddr := uCar(cdr), uCdr(cdr) // Checked at the top of this loop
		if !(instance.Of(class.Cons, cadr) && instance.Of(class.Cons, uCdr(cadr)) && uCdr(uCdr(cadr)) == Nil) {
			return nil, instance.New(class.ProgramError)
		}
		v := uCar(cadr) // Checked before statement
		f, err := Eval(local, global, uCar(uCdr(cadr)))
		if err != nil {
			return nil, err
		}
		if local.Variable.Define(v, f) {
			return nil, instance.New(class.ProgramError)
		}
		cdr = cddr
	}
	var err ilos.Instance
	ret := Nil
	for _, form := range bodyForm {
		ret, err = Eval(local, global, form)
		if err != nil {
			return nil, err
		}
	}
	return ret, nil
}
