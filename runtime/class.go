// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"fmt"
	"reflect"

	"github.com/islisp-dev/iris/runtime/ilos"
)

func ClassOf(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return obj.Class(), nil
}

func Instancep(e ilos.Environment, obj ilos.Instance, class ilos.Class) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(class, obj) {
		return T, nil
	}
	return Nil, nil
}

func Subclassp(e ilos.Environment, class1, class2 ilos.Class) (ilos.Instance, ilos.Instance) {
	if ilos.SubclassOf(class1, class2) {
		return T, nil
	}
	return Nil, nil
}

func Class(e ilos.Environment, className ilos.Instance) (ilos.Class, ilos.Instance) {
	if v, ok := e.Class[:1].Get(className); ok {
		return v.(ilos.Class), nil
	}
	_, err := SignalCondition(e, ilos.NewUndefinedClass(e, className), Nil)
	return nil, err
}

func checkSuperClass(a, b ilos.Class) bool {
	if reflect.DeepEqual(a, ilos.StandardObjectClass) || reflect.DeepEqual(b, ilos.StandardObjectClass) {
		return false
	}
	if ilos.SubclassOf(a, b) || ilos.SubclassOf(b, a) {
		return true
	}
	for _, c := range a.Supers() {
		if checkSuperClass(c, b) {
			return true
		}
	}
	for _, c := range b.Supers() {
		if checkSuperClass(a, c) {
			return true
		}
	}
	return false
}

func Defclass(e ilos.Environment, className, scNames, slotSpecs ilos.Instance, classOpts ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.SymbolClass, className); err != nil {
		return nil, err
	}
	if err := ensure(e, ilos.ListClass, scNames, slotSpecs); err != nil {
		return nil, err
	}
	supers := []ilos.Class{ilos.StandardObjectClass}
	for _, scName := range scNames.(ilos.List).Slice() {
		super, err := Class(e, scName)
		if err != nil {
			return nil, err
		}
		for _, before := range supers {
			if checkSuperClass(before, super) {
				return SignalCondition(e, ilos.NewArityError(e), Nil)
			}
		}
		supers = append(supers, super)
	}
	slots := []ilos.Instance{}
	initforms := map[ilos.Instance]ilos.Instance{}
	initargs := map[ilos.Instance]ilos.Instance{}
	for _, slotSpec := range slotSpecs.(ilos.List).Slice() {
		if ilos.InstanceOf(ilos.SymbolClass, slotSpec) {
			slotName := slotSpec
			slots = append(slots, slotName)
			continue
		}
		slotName := slotSpec.(*ilos.Cons).Car
		slots = append(slots, slotName)
		slotOpts := slotSpec.(*ilos.Cons).Cdr.(ilos.List).Slice()
		for i := 0; i < len(slotOpts); i += 2 {
			switch slotOpts[i] {
			case ilos.NewSymbol(":INITFORM"):
				closure, err := newNamedFunction(e, ilos.NewSymbol("CLOSURE"), Nil, slotOpts[i+1])
				if err != nil {
					return nil, err
				}
				initforms[slotName] = closure
			case ilos.NewSymbol(":INITARG"):
				initargs[slotOpts[i+1]] = slotName
			}
		}
	}
	metaclass := ilos.StandardClassClass
	abstractp := Nil
	for _, classOpt := range classOpts {
		var err ilos.Instance
		switch classOpt.(*ilos.Cons).Car {
		case ilos.NewSymbol(":METACLASS"):
			if metaclass, err = Class(e, classOpt.(ilos.List).Nth(1)); err != nil {
				return nil, err
			}
		case ilos.NewSymbol(":ABSTRACTP"):
			if abstractp, err = Eval(e, classOpt.(ilos.List).Nth(1)); err != nil {
				return nil, err
			}
		}
	}
	classObject := ilos.NewStandardClass(className, supers, slots, initforms, initargs, metaclass, abstractp)
	e.Class[:1].Define(className, classObject)
	for _, slotSpec := range slotSpecs.(ilos.List).Slice() {
		if ilos.InstanceOf(ilos.SymbolClass, slotSpec) {
			continue
		}
		slotName := slotSpec.(*ilos.Cons).Car
		slotOpts := slotSpec.(*ilos.Cons).Cdr.(ilos.List).Slice()
		var readerFunctionName, writerFunctionName, boundpFunctionName ilos.Instance
		for i := 0; i < len(slotOpts); i += 2 {
			switch slotOpts[i] {
			case ilos.NewSymbol(":READER"):
				readerFunctionName = slotOpts[i+1]
			case ilos.NewSymbol(":WRITER"):
				writerFunctionName = slotOpts[i+1]
			case ilos.NewSymbol(":ACCESSOR"):
				readerFunctionName = slotOpts[i+1]
				writerFunctionName = ilos.NewSymbol(fmt.Sprintf("(SETF %v)", slotOpts[i+1]))
			case ilos.NewSymbol(":BOUNDP"):
				boundpFunctionName = slotOpts[i+1]
			}
		}
		if readerFunctionName != nil {
			lambdaList, err := List(e, ilos.NewSymbol("INSTANCE"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(readerFunctionName); !ok || !ilos.InstanceOf(ilos.GenericFunctionClass, g) {
				Defgeneric(e, readerFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(readerFunctionName)
			fun.(*ilos.GenericFunction).AddMethod(nil, lambdaList, []ilos.Class{classObject}, ilos.NewFunction(readerFunctionName, func(e ilos.Environment, object ilos.Instance) (ilos.Instance, ilos.Instance) {
				slot, ok := object.(ilos.BasicInstance).GetSlotValue(slotName, classObject)
				if ok {
					return slot, nil
				}
				return Nil, nil // TODO: shoud throw an error.
			}))
		}
		if writerFunctionName != nil {
			lambdaList, err := List(e, ilos.NewSymbol("Y"), ilos.NewSymbol("X"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(writerFunctionName); !ok || !ilos.InstanceOf(ilos.GenericFunctionClass, g) {
				Defgeneric(e, writerFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(writerFunctionName)
			fun.(*ilos.GenericFunction).AddMethod(nil, lambdaList, []ilos.Class{ilos.ObjectClass, classObject}, ilos.NewFunction(writerFunctionName, func(e ilos.Environment, obj, object ilos.Instance) (ilos.Instance, ilos.Instance) {
				ok := object.(ilos.BasicInstance).SetSlotValue(obj, slotName, classObject)
				if ok {
					return obj, nil
				}
				return Nil, nil
			}))
		}
		if boundpFunctionName != nil {
			lambdaList, err := List(e, ilos.NewSymbol("INSTANCE"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(boundpFunctionName); !ok || !ilos.InstanceOf(ilos.GenericFunctionClass, g) {
				Defgeneric(e, boundpFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(boundpFunctionName)
			fun.(*ilos.GenericFunction).AddMethod(nil, lambdaList, []ilos.Class{classObject}, ilos.NewFunction(boundpFunctionName, func(e ilos.Environment, object ilos.Instance) (ilos.Instance, ilos.Instance) {
				_, ok := object.(ilos.BasicInstance).GetSlotValue(slotName, classObject)
				if ok {
					return T, nil
				}
				return Nil, nil
			}))
		}
	}
	return className, nil
}

func Create(e ilos.Environment, c ilos.Instance, i ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.StandardClassClass, c); err != nil {
		return nil, err
	}
	return ilos.Create(e, c, i...), nil
}

func InitializeObject(e ilos.Environment, object ilos.Instance, inits ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.StandardObjectClass, object); err != nil {
		return nil, err
	}
	return ilos.InitializeObject(e, object, inits...), nil
}

func Defmethod(e ilos.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if len(arguments) < 2 {
		return SignalCondition(e, ilos.NewArityError(e), Nil)
	}
	name := arguments[0]
	var qualifier ilos.Instance
	i := 0
	if arguments[1] == ilos.NewSymbol(":AROUND") || arguments[1] == ilos.NewSymbol(":BEFORE") || arguments[1] == ilos.NewSymbol(":AFTER") {
		qualifier = arguments[1]
		i++
	}
	parameterList := []ilos.Instance{}
	for _, pp := range arguments[i+1].(ilos.List).Slice() {
		if ilos.InstanceOf(ilos.SymbolClass, pp) {
			parameterList = append(parameterList, pp)
		} else {
			parameterList = append(parameterList, pp.(ilos.List).Nth(0))
		}
	}
	lambdaList, err := List(e, parameterList...)
	if err != nil {
		return nil, err
	}
	classList := []ilos.Class{}
	for _, pp := range arguments[i+1].(ilos.List).Slice() {
		if pp == ilos.NewSymbol(":REST") && pp == ilos.NewSymbol("&REST") {
			break
		}
		if ilos.InstanceOf(ilos.SymbolClass, pp) {
			classList = append(classList, ilos.ObjectClass)
		} else {
			class, ok := e.Class[:1].Get(pp.(ilos.List).Nth(1))
			if !ok {
				return SignalCondition(e, ilos.NewUndefinedClass(e, pp.(ilos.List).Nth(1)), Nil)

			}
			classList = append(classList, class.(ilos.Class))
		}
	}
	fun, err := newNamedFunction(e, name, lambdaList, arguments[i+2:]...)
	if err != nil {
		return nil, err
	}
	gen, ok := e.Function[:1].Get(name)
	if !ok {
		return SignalCondition(e, ilos.NewUndefinedFunction(e, name), Nil)
	}
	if !gen.(*ilos.GenericFunction).AddMethod(qualifier, lambdaList, classList, fun) {
		return SignalCondition(e, ilos.NewUndefinedFunction(e, name), Nil)
	}
	return name, nil
}

func Defgeneric(e ilos.Environment, funcSpec, lambdaList ilos.Instance, optionsOrMethodDescs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var methodCombination ilos.Instance
	genericFunctionClass := ilos.StandardGenericFunctionClass
	forms := []ilos.Instance{}
	for _, optionOrMethodDesc := range optionsOrMethodDescs {
		switch optionOrMethodDesc.(ilos.List).Nth(0) {
		case ilos.NewSymbol(":METHOD-COMBINATION"):
			methodCombination = optionOrMethodDesc.(ilos.List).Nth(1)
		case ilos.NewSymbol(":GENERIC-FUNCTION-CLASS"):
			class, ok := e.Class[:1].Get(optionOrMethodDesc.(ilos.List).Nth(1))
			if !ok {
				return SignalCondition(e, ilos.NewUndefinedClass(e, optionOrMethodDesc.(ilos.List).Nth(1)), Nil)
			}
			genericFunctionClass = class.(ilos.Class)
		case ilos.NewSymbol(":METHOD"):
			forms = append(forms, ilos.NewCons(ilos.NewSymbol("DEFMETHOD"), optionOrMethodDesc.(ilos.List).NthCdr(1)))
		}
	}
	e.Function[:1].Define(
		ilos.NewSymbol(
			fmt.Sprint(funcSpec),
		),
		ilos.NewGenericFunction(
			funcSpec,
			lambdaList,
			methodCombination,
			genericFunctionClass,
		),
	)
	Progn(e, forms...)
	return funcSpec, nil
}

func GenericFunctionP(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(ilos.GenericFunctionClass, obj) {
		return T, nil
	}
	return Nil, nil
}
