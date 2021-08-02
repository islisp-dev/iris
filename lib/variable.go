// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import (
	"fmt"

	"github.com/islisp-dev/iris/core"
)

// Setq represents an assignment to the variable denoted by the identifier. In
// consequence, the identifier may designate a different object than before, the
// value of form. The result of the evaluation of form is returned. This result
// is used to modify the variable binding denoted by the identifier var (if it
// is mutable). setq can be used only for modifying bindings, and not for
// establishing a variable. The setq special form must be contained in the scope
// of var , established by defglobal, let, let*, for, or a lambda expression.
func Setq(e core.Environment, var1, form core.Instance) (core.Instance, core.Instance) {
	ret, err := Eval(e, form)
	if err != nil {
		return nil, err
	}
	if e.Variable.Set(var1, ret) {
		return ret, nil
	}
	return SignalCondition(e, core.NewUnboundVariable(e, var1), Nil)
}

func Setf(e core.Environment, var1, form core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.SymbolClass, var1) {
		return Setq(e, var1, form)
	}
	if !core.InstanceOf(core.ListClass, var1) {
		return SignalCondition(e, core.NewParseError(e, var1, core.ListClass), Nil)
	}
	funcSpec := core.NewSymbol(fmt.Sprintf("(SETF %v)", var1.(core.List).Nth(0)))
	fun, ok := e.Function.Get(funcSpec)
	if !ok {
		return SignalCondition(e, core.NewUndefinedFunction(e, funcSpec), Nil)
	}
	arguments, err := evalArguments(e, core.NewCons(form, var1.(*core.Cons).Cdr))
	if err != nil {
		return nil, err
	}
	return fun.(core.Applicable).Apply(e, arguments.(core.List).Slice()...)
}

// Let is used to define a scope for a group of identifiers for a sequence of
// forms body-form* (collectively referred to as the body). The list of pairs
// (var form)* is called the let variable list. The scope of the identifier var
// is the body. The forms are evaluated sequentially from left to right; then
// each variable denoted by the identifier var is initialized to the
// corresponding value. Using these bindings along with the already existing
// bindings of visible identifiers the body-forms are evaluated. The returned
// value of let is the result of the evaluation of the last body-form of its
// body (or nil if there is none). No var may appear more than once in let
// variable list.

func Let(e core.Environment, varForm core.Instance, bodyForm ...core.Instance) (core.Instance, core.Instance) {
	if !Rep(Tpl(Sym(), Any))(varForm) {
		return SignalCondition(e, core.NewDomainError(e, varForm, core.ListClass), Nil)
	}
	vfs := map[core.Instance]core.Instance{}
	for _, cadr := range varForm.(core.List).Slice() {
		if err := ensure(e, core.ListClass, cadr); err != nil {
			return nil, err
		}
		if cadr.(core.List).Length() != 2 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		f, err := Eval(e, cadr.(core.List).Nth(1))
		if err != nil {
			return nil, err
		}
		vfs[cadr.(core.List).Nth(0)] = f
	}
	for v, f := range vfs {
		if !e.Variable.Define(v, f) {
			return SignalCondition(e, core.NewImmutableBinding(e), Nil)
		}
	}
	return Progn(e, bodyForm...)
}

// LetStar form is used to define a scope for a group of identifiers for a
// sequence of forms body-form* (collectively referred to as the body). The
// first subform (the let* variable list) is a list of pairs (var form). The
// scope of an identifier var is the body along with all form forms following
// the pair (var form) in the let* variable list. For each pair (var form) the
// following is done: form is evaluated in the context of the bindings in effect
// at that point in the evaluation. The result of the evaluation is bound to its
// associated variable named by the identifier var . These variable bindings
// enlarge the set of current valid identifiers perhaps shadowing previous
// variable bindings (in case some var was defined outside), and in this
// enlarged or modified eironment the body-forms are executed. The returned
// value of let* is the result of the evaluation of the last form of its body
// (or nil if there is none).
func LetStar(e core.Environment, varForm core.Instance, bodyForm ...core.Instance) (core.Instance, core.Instance) {
	if !Rep(Tpl(Sym(), Any))(varForm) {
		return SignalCondition(e, core.NewDomainError(e, varForm, core.ListClass), Nil)
	}
	for _, cadr := range varForm.(core.List).Slice() {
		if err := ensure(e, core.ListClass, cadr); err != nil {
			return nil, err
		}
		if cadr.(core.List).Length() != 2 {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		f, err := Eval(e, cadr.(core.List).Nth(1))
		if err != nil {
			return nil, err
		}
		if !e.Variable.Define(cadr.(core.List).Nth(0), f) {
			return SignalCondition(e, core.NewImmutableBinding(e), Nil)
		}
	}
	return Progn(e, bodyForm...)
}
