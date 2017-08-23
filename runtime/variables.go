// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"fmt"

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
func Setq(local environment.Environment, var1, form ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := Eval(local, form)
	if err != nil {
		return nil, err
	}
	if local.Variable.Set(var1, ret) {
		return ret, nil
	}
	if local.Variable.Set(var1, ret) {
		return ret, nil
	}
	return nil, instance.NewUndefinedVariable(var1)
}

func Setf(local environment.Environment, var1, form ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(class.Symbol, var1) {
		val, err := Eval(local, form)
		if err != nil {
			return nil, err
		}
		return Setq(local, var1, val)
	}
	funcSpec := instance.NewSymbol(fmt.Sprintf("(SETF %v)", var1.(instance.List).Nth(0)))
	fun, ok := local.Function.Get(funcSpec)
	if !ok {
		return nil, instance.NewUndefinedFunction(funcSpec)
	}
	arguments, err := evalArguments(local, instance.NewCons(form, var1.(*instance.Cons).Cdr))
	if err != nil {
		return nil, err
	}
	return fun.(instance.Applicable).Apply(local, arguments.(instance.List).Slice()...)
}

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
func Let(local environment.Environment, varForm ilos.Instance, bodyForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	vfs := map[ilos.Instance]ilos.Instance{}
	if err := ensure(class.List, varForm); err != nil {
		return nil, err
	}
	for _, cadr := range varForm.(instance.List).Slice() {
		if err := ensure(class.List, cadr); err != nil {
			return nil, err
		}
		if cadr.(instance.List).Length() != 2 {
			return nil, instance.NewArityError()
		}
		f, err := Eval(local, cadr.(instance.List).Nth(1))
		if err != nil {
			return nil, err
		}
		vfs[cadr.(instance.List).Nth(0)] = f
	}
	for v, f := range vfs {
		if !local.Variable.Define(v, f) {
			return nil, instance.NewImmutableBinding()
		}
	}
	return Progn(local, bodyForm...)
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
func LetStar(local environment.Environment, varForm ilos.Instance, bodyForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.List, varForm); err != nil {
		return nil, err
	}
	for _, cadr := range varForm.(instance.List).Slice() {
		if err := ensure(class.List, cadr); err != nil {
			return nil, err
		}
		if cadr.(instance.List).Length() != 2 {
			return nil, instance.NewArityError()
		}
		f, err := Eval(local, cadr.(instance.List).Nth(1))
		if err != nil {
			return nil, err
		}
		if !local.Variable.Define(cadr.(instance.List).Nth(0), f) {
			return nil, instance.NewImmutableBinding()
		}
	}
	return Progn(local, bodyForm...)
}
