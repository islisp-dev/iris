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

// TODO: setf dynamic

// Dynamic denotes a reference to the identifier denoting a
// dynamic variable. This special form is not allowed in the scope of a
// definition of var which is not done by defdynamic or dynamic-let.
//
// During activation, the current dynamic binding of the variable var is
// returned that was established most recently and is still in effect. An
// error shall be signaled if such a binding does not exist
// (error-id. unbound-variable).
func Dynamic(local, global *environment.Environment, var1 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Symbol, var1); err != nil {
		return nil, err
	}
	if v, ok := local.DynamicVariable.Get(var1); ok {
		return v, nil
	}
	if v, ok := global.DynamicVariable.Get(var1); ok {
		return v, nil
	}
	return nil, instance.NewUndefinedVariable(var1)
}

// SetDynamic denotes an assignment to a dynamic variable. This
// form can appear anywhere that (dynamic var) can appear.
//
// form is evaluated and the result of the evaluation is used to change
// the dynamic binding of var.
//
// An error shall be signaled if var has no dynamic value
// (error-id.  unbound-variable). setf of dynamic can be used only for
// modifying bindings, and not for establishing them.
func SetDynamic(local, global *environment.Environment, form, var1 ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Symbol, var1); err != nil {
		return nil, err
	}
	form, err := Eval(local, global, form)
	if err != nil {
		return nil, form
	}
	if local.DynamicVariable.Set(var1, form) {
		return form, nil
	}
	if global.DynamicVariable.Set(var1, form) {
		return form, nil
	}
	return nil, instance.NewUndefinedVariable(var1)
}

// DynamicLet is used to establish dynamic variable bindings.
// The first subform (the dynamic-let variable list) is a list of pairs (var
// form). The scope of an identifier var defined by dynamic-let is the current
// toplevel scope. The extent of the bindings of each var is the extent of the
// body of the dynamic-let. The dynamic-let special form establishes dynamic
// variables for all vars.
//
// References to a dynamic variable named by var must be made through the
// dynamic special form.
//
// All the initializing forms are evaluated sequentially from left to right, and
// then the values are associated with the corresponding vars. Using these
// additional dynamic bindings and the already existing bindings of visible
// identifiers, the forms body-form* are evaluated in sequential order. The
// returned value of dynamic-let is that of the last body-form of the body (or
// nil if there is none). The bindings are undone when control leaves the
// prepared dynamic-let special form.
func DynamicLet(local, global *environment.Environment, varForm ilos.Instance, bodyForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	vfs := map[ilos.Instance]ilos.Instance{}
	if err := ensure(class.List, varForm); err != nil {
		return nil, err
	}
	for _, cadr := range varForm.(instance.List).Slice() {
		if err := ensure(class.List, cadr); err != nil {
			return nil, err
		}
		s := cadr.(instance.List).Slice()
		if len(s) != 2 {
			return nil, instance.NewArityError()
		}
		f, err := Eval(local, global, s[1])
		if err != nil {
			return nil, err
		}
		vfs[s[0]] = f
	}
	for v, f := range vfs {
		if !local.DynamicVariable.Define(v, f) {
			return nil, instance.NewImmutableBinding()
		}
	}
	return Progn(local, global, bodyForm...)
}
