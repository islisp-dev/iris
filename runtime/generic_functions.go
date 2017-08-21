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

func Defmethod(local, global environment.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if len(arguments) < 2 {
		return nil, instance.NewArityError()
	}
	name := arguments[0]
	var qualifier ilos.Instance
	i := 0
	if arguments[1] == instance.NewSymbol(":AROUND") || arguments[1] == instance.NewSymbol(":BEFORE") || arguments[1] == instance.NewSymbol(":AFTER") {
		qualifier = arguments[1]
		i++
	}
	parameterList := []ilos.Instance{}
	for _, pp := range arguments[i+1].(instance.List).Slice() {
		if ilos.InstanceOf(class.Symbol, pp) {
			parameterList = append(parameterList, pp)
		} else {
			parameterList = append(parameterList, pp.(instance.List).Nth(0))
		}
	}
	lambdaList, err := List(local, global, parameterList...)
	if err != nil {
		return nil, err
	}
	classList := []ilos.Class{}
	for _, pp := range arguments[i+1].(instance.List).Slice() {
		if pp == instance.NewSymbol(":REST") && pp == instance.NewSymbol("&REST") {
			break
		}
		if ilos.InstanceOf(class.Symbol, pp) {
			classList = append(classList, class.Object)
		} else {
			class, ok := global.Class.Get(pp.(instance.List).Nth(1))
			if !ok {
				return nil, instance.NewUndefinedClass(pp.(instance.List).Nth(1))
			}
			classList = append(classList, class.(ilos.Class))
		}
	}
	fun, err := newNamedFunction(local, global, name, lambdaList, arguments[i+2:]...)
	if err != nil {
		return nil, err
	}
	gen, ok := global.Function.Get(name)
	if !ok {
		return nil, instance.NewUndefinedFunction(name)
	}
	if !gen.(*instance.GenericFunction).AddMethod(qualifier, lambdaList, classList, fun) {
		return nil, instance.NewUndefinedFunction(name)
	}
	return name, nil
}

func Defgeneric(local, global environment.Environment, funcSpec, lambdaList ilos.Instance, optionsOrMethodDescs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var methodCombination ilos.Instance
	genericFunctionClass := class.StandardGenericFunction
	forms := []ilos.Instance{}
	for _, optionOrMethodDesc := range optionsOrMethodDescs {
		switch optionOrMethodDesc.(instance.List).Nth(0) {
		case instance.NewSymbol(":METHOD-COMBINATION"):
			methodCombination = optionOrMethodDesc.(instance.List).Nth(1)
		case instance.NewSymbol(":GENERIC-FUNCTION-CLASS"):
			class, ok := global.Class.Get(optionOrMethodDesc.(instance.List).Nth(1))
			if !ok {
				return nil, instance.NewUndefinedClass(optionOrMethodDesc.(instance.List).Nth(1))
			}
			genericFunctionClass = class.(ilos.Class)
		case instance.NewSymbol(":METHOD"):
			forms = append(forms, instance.NewCons(instance.NewSymbol("DEFMETHOD"), optionOrMethodDesc.(instance.List).NthCdr(1)))
		}
	}
	global.Function.Define(funcSpec, instance.NewGenericFunction(funcSpec, lambdaList, methodCombination, genericFunctionClass))
	Progn(local, global, forms...)
	return funcSpec, nil
}
