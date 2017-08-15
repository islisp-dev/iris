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

// Functionp returns t if obj is a (normal or generic) function;
// otherwise, returns nil. obj may be any ISLISP object.
//
// Function bindings are entities established during execution of
// a prepared labels or flet forms or by a function-defining form.
// A function binding is an association between an identifier, function-name,
// and a function object that is denoted by function-name—if in operator
// position—or by (function function-name) elsewhere.
func Functionp(local, global *environment.Environment, fun ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Function, fun) {
		return T, nil
	}
	return Nil, nil
}

// Function returns the function object named by function-name.
//
// An error shall be signaled if no binding has been established for the identifier
// in the function namespace of current lexical environment (error-id. undefined-function).
// The consequences are undefined if the function-name names a macro or special form
func Function(local, global *environment.Environment, fun ilos.Instance) (ilos.Instance, ilos.Instance) {
	// car must be a symbol
	if err := ensure(class.Symbol, fun); err != nil {
		return nil, err
	}
	if f, ok := local.Function.Get(fun); ok {
		return f, nil
	}
	if f, ok := global.Function.Get(fun); ok {
		return f, nil
	}
	return nil, instance.New(class.UndefinedFunction, map[string]ilos.Instance{
		"NAME":      fun,
		"NAMESPACE": instance.NewSymbol("FUNCTION"),
	})
}

// Lambda special form creates a function object.
//
// The scope of the identifiers of the lambda-list is the sequence of forms form*,
// collectively referred to as the body.
//
// When the prepared function is activated later (even if transported as object
// to some other activation) with some arguments, the body of the function is
// evaluated as if it was at the same textual position where the lambda special
// form is located, but in a context where the lambda variables are bound
// in the variable namespace with the values of the corresponding arguments.
// A &rest or :rest variable, if any, is bound to the list of the values of
// the remaining arguments. An error shall be signaled if the number of
// arguments received is incompatible with the specified lambda-list
// (error-id. arity-error).
//
// Once the lambda variables have been bound, the body is executed.
// If the body is empty, nil is returned otherwise the result of the evaluation of
// the last form of body is returned if the body was not left by a non-local exit.
//
// If the function receives a &rest or :rest parameter R, the list L1 to which that
// parameter is bound has indefinite extent. L1 is newly allocated unless the function
// was called with apply and R corresponds to the final argument, L2 , to that call
// to apply (or some subtail of L2), in which case it is implementation defined whether
// L1 shares structure with L2 .
func Lambda(local, global *environment.Environment, lambdaList ilos.Instance, form ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := checkLambdaList(lambdaList); err != nil {
		return nil, err
	}
	return newNamedFunction(local, global, instance.NewSymbol("ANONYMOUS-FUNCTION"), lambdaList, form...)
}

// Labels special form allow the definition of new identifiers in the function
// namespace for function objects.
//
// In a labels special form the scope of an identifier function-name is the whole
// labels special form (excluding nested scopes, if any); for the flet special form,
// the scope of an identifier is only the body-form*. Within these scopes,
// each function-name is bound to a function object whose behavior is equivalent
// to (lambda lambda-list form*), where free identifier references are resolved as follows:
//
// For a labels form, such free references are resolved in the lexical environment
// that was active immediately outside the labels form augmented by the function
// bindings for the given function-names (i.e., any reference to a function
// function-name refers to a binding created by the labels).
//
// For a flet form, free identifier references in the lambda-expression are resolved
// in the lexical environment that was active immediately outside the flet form
// (i.e., any reference to a function function-name are not visible).
//
// During activation, the prepared labels or flet establishes function bindings and
// then evaluates each body-form in the body sequentially; the value of the last one
// (or nil if there is none) is the value returned by the special form activation.
//
// No function-name may appear more than once in the function bindings.
func Labels(local, global *environment.Environment, functions ilos.Instance, bodyForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.List, functions); err != nil {
		return nil, err
	}
	for _, function := range functions.(instance.List).Slice() {
		if err := ensure(class.List, function); err != nil {
			return nil, err
		}
		definition := function.(instance.List).Slice()
		if len(definition) < 2 {
			return nil, instance.NewArityError()
		}
		functionName := definition[0]
		lambdaList := definition[1]
		forms := definition[2:]
		fun, err := newNamedFunction(local, global, functionName, lambdaList, forms...)
		if err != nil {
			return nil, err
		}
		if !local.Function.Define(functionName, fun) {
			return nil, instance.NewImmutableBinding()
		}
	}
	return Progn(local, global, bodyForm...)
}

// Flet special form allow the definition of new identifiers in the function
// namespace for function objects (see Labels).
func Flet(local, global *environment.Environment, functions ilos.Instance, bodyForm ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.List, functions); err != nil {
		return nil, err
	}
	env := environment.New().Merge(local)
	for _, function := range functions.(instance.List).Slice() {
		if err := ensure(class.List, function); err != nil {
			return nil, err
		}
		definition := function.(instance.List).Slice()
		if len(definition) < 2 {
			return nil, instance.NewArityError()
		}
		functionName := definition[0]
		lambdaList := definition[1]
		forms := definition[2:]
		fun, err := newNamedFunction(local, global, functionName, lambdaList, forms...)
		if err != nil {
			return nil, err
		}
		if !env.Function.Define(functionName, fun) {
			return nil, instance.NewImmutableBinding()
		}
	}
	return Progn(env, global, bodyForm...)
}

// Apply applies function to the arguments, obj*, followed by the elements of list,
// if any. It returns the value returned by function.
//
// An error shall be signaled if function is not a function (error-id. domain-error).
// Each obj may be any ISLISP object. An error shall be signaled
// if list is not a proper list (error-id. improper-argument-list).
func Apply(local, global *environment.Environment, function ilos.Instance, obj ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Function, function); err != nil {
		return nil, err
	}
	if err := ensure(class.List, obj[len(obj)-1]); err != nil {
		return nil, err
	}
	obj = append(obj[:len(obj)-1], obj[len(obj)-1].(instance.List).Slice()...)
	return function.(instance.Applicable).Apply(local, global, obj...)
}

// Funcall activates the specified function function and returns the value that the function returns.
// The ith argument (2 ≤ i) of funcall becomes the (i − 1)th argument of the function.
//
// An error shall be signaled if function is not a function (error-id. domain-error).
// Each obj may be any ISLISP object.
func Funcall(local, global *environment.Environment, function ilos.Instance, obj ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	obj = append(obj, Nil)
	return Apply(local, global, function, obj...)
}
