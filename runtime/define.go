// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/env"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

// Defconstant is used to define a named constant in the variable namespace of
// the current toplevel scope. The scope of name is the entire current toplevel
// scope except the body form. Although name is globally constant, a variable
// binding for name can be ely established by a binding form. The result of the
// evaluation of form is bound to the variable named by name. The binding and
// the object created as the result of evaluating the second argument are
// immutable. The symbol named name is returned.
func Defconstant(e env.Environment, name, form ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.SymbolClass, name); err != nil {
		return nil, err
	}
	if _, ok := e.Constant[:1].Get(name); ok {
		return SignalCondition(e, instance.NewImmutableBinding(e), Nil)
	}
	ret, err := Eval(e, form)
	if err != nil {
		return nil, err
	}
	e.Constant[:1].Define(name, ret)
	return name, nil
}

// Defglobal is used to define an identifier in the variable namespace of the
// current toplevel scope. The scope of name is the entire current toplevel
// scope except the body form. form is evaluated to compute an initializing
// value for the variable named name. Therefore, defglobal is used only for
// defining variables and not for modifying them. The symbol named name is
// returned. A lexical variable binding for name can still be ely established by
// a binding form; in that case, the e binding lexically shadows the outer
// binding of name defined by defe.
func Defglobal(e env.Environment, name, form ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.SymbolClass, name); err != nil {
		return nil, err
	}
	if _, ok := e.Constant[:1].Get(name); ok {
		return SignalCondition(e, instance.NewImmutableBinding(e), Nil)
	}
	ret, err := Eval(e, form)
	if err != nil {
		return nil, err
	}
	e.Variable[:1].Define(name, ret)
	return name, nil
}

// Defdynamic is used to define a dynamic variable identifier in the dynamic
// variable namespace. The scope of name is the entire current toplevel scope
// except the body form.The symbol named name is returned.
func Defdynamic(e env.Environment, name, form ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.SymbolClass, name); err != nil {
		return nil, err
	}
	if _, ok := e.Constant[:1].Get(name); ok {
		return SignalCondition(e, instance.NewImmutableBinding(e), Nil)
	}
	ret, err := Eval(e, form)
	if err != nil {
		return nil, err
	}
	e.DynamicVariable[:1].Define(name, ret)
	return name, nil
}

// Defun defines function-name as an identifier in the function namespace;
// function-name is bound to a function object equivalent to (lambda lambda-list
// form*). The scope of function-name is the whole current toplevel scope.
// Therefore, the definition of a function admits recursion, occurrences of
// function-name within the form* refer to the function being defined. The
// binding between function-name and the function object is immutable. defun
// returns the function name which is the symbol named function-name. The free
// identifiers in the body form* (i.e., those which are not contained in the
// lambda list) follow the rules of lexical scoping.
func Defun(e env.Environment, functionName, lambdaList ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.SymbolClass, functionName); err != nil {
		return nil, err
	}
	ret, err := newNamedFunction(e, functionName, lambdaList, forms...)
	if err != nil {
		return nil, err
	}
	e.Function[:1].Define(functionName, ret)
	return functionName, nil
}
