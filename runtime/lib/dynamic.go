// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/runtime/core"

// Dynamic denotes a reference to the identifier denoting a dynamic variable.
// This special form is not allowed in the scope of a definition of var which is
// not done by defdynamic or dynamic-let. During activation, the current dynamic
// binding of the variable var is returned that was established most recently
// and is still in effect. An error shall be signaled if such a binding does not
// exist (error-id. unbound-variable).
func Dynamic(e core.Environment, var1 core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.SymbolClass, var1); err != nil {
		return nil, err
	}
	if v, ok := e.DynamicVariable.Get(var1); ok {
		return v, nil
	}
	if v, ok := e.DynamicVariable.Get(var1); ok {
		return v, nil
	}
	return SignalCondition(e, core.NewUnboundVariable(e, var1), Nil)
}

// SetDynamic denotes an assignment to a dynamic variable. This form can appear
// anywhere that (dynamic var) can appear. form is evaluated and the result of
// the evaluation is used to change the dynamic binding of var. An error shall
// be signaled if var has no dynamic value (error-id.  unbound-variable). setf
// of dynamic can be used only for modifying bindings, and not for establishing
// them.
func SetDynamic(e core.Environment, form, var1 core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.SymbolClass, var1); err != nil {
		return nil, err
	}
	form, err := Eval(e, form)
	if err != nil {
		return nil, form
	}
	if e.DynamicVariable.Set(var1, form) {
		return form, nil
	}
	if e.DynamicVariable.Set(var1, form) {
		return form, nil
	}
	return SignalCondition(e, core.NewUnboundVariable(e, var1), Nil)
}

// DynamicLet is used to establish dynamic variable bindings. The first subform
// (the dynamic-let variable list) is a list of pairs (var form). The scope of
// an identifier var defined by dynamic-let is the current toplevel scope. The
// extent of the bindings of each var is the extent of the body of the
// dynamic-let. The dynamic-let special form establishes dynamic variables for
// all vars. References to a dynamic variable named by var must be made through
// the dynamic special form. All the initializing forms are evaluated
// sequentially from left to right, and then the values are associated with the
// corresponding vars. Using these additional dynamic bindings and the already
// existing bindings of visible identifiers, the forms body-form* are evaluated
// in sequential order. The returned value of dynamic-let is that of the last
// body-form of the body (or nil if there is none). The bindings are undone when
// control leaves the prepared dynamic-let special form.
func DynamicLet(e core.Environment, varForm core.Instance, bodyForm ...core.Instance) (core.Instance, core.Instance) {
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
		if !e.DynamicVariable.Define(v, f) {
			return SignalCondition(e, core.NewImmutableBinding(e), Nil)
		}
	}
	return Progn(e, bodyForm...)
}
