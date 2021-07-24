// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import (
	"fmt"

	"github.com/islisp-dev/iris/runtime/core"
)

func ClassOf(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	return obj.Class(), nil
}

func Instancep(e core.Environment, obj core.Instance, class core.Class) (core.Instance, core.Instance) {
	if core.InstanceOf(class, obj) {
		return T, nil
	}
	return Nil, nil
}

func Subclassp(e core.Environment, class1, class2 core.Class) (core.Instance, core.Instance) {
	if core.SubclassOf(class1, class2) {
		return T, nil
	}
	return Nil, nil
}

func Class(e core.Environment, className core.Instance) (core.Class, core.Instance) {
	if v, ok := e.Class[:1].Get(className); ok {
		return v.(core.Class), nil
	}
	_, err := SignalCondition(e, core.NewUndefinedClass(e, className), Nil)
	return nil, err
}

func checkSuperClass(a, b core.Class) bool {
	if core.DeepEqual(a, core.StandardObjectClass) || core.DeepEqual(b, core.StandardObjectClass) {
		return false
	}
	if core.SubclassOf(a, b) || core.SubclassOf(b, a) {
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

func Defclass(e core.Environment, className, scNames, slotSpecs core.Instance, classOpts ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.SymbolClass, className); err != nil {
		return nil, err
	}
	if err := ensure(e, core.ListClass, scNames, slotSpecs); err != nil {
		return nil, err
	}
	supers := []core.Class{core.StandardObjectClass}
	for _, scName := range scNames.(core.List).Slice() {
		super, err := Class(e, scName)
		if err != nil {
			return nil, err
		}
		for _, before := range supers {
			if checkSuperClass(before, super) {
				return SignalCondition(e, core.NewArityError(e), Nil)
			}
		}
		supers = append(supers, super)
	}
	slots := []core.Instance{}
	initforms := core.NewAssociateList()
	initargs := core.NewAssociateList()
	for _, slotSpec := range slotSpecs.(core.List).Slice() {
		if core.InstanceOf(core.SymbolClass, slotSpec) {
			slotName := slotSpec
			slots = append(slots, slotName)
			continue
		}
		slotName := slotSpec.(*core.Cons).Car
		slots = append(slots, slotName)
		slotOpts := slotSpec.(*core.Cons).Cdr.(core.List).Slice()
		for i := 0; i < len(slotOpts); i += 2 {
			switch {
			case core.DeepEqual(slotOpts[i], core.NewSymbol(":INITFORM")):
				closure, err := newNamedFunction(e, core.NewSymbol("CLOSURE"), Nil, slotOpts[i+1])
				if err != nil {
					return nil, err
				}
				initforms.Set(slotName, closure)
			case core.DeepEqual(slotOpts[i], core.NewSymbol(":INITARG")):
				initargs.Set(slotOpts[i+1], slotName)
			}
		}
	}
	metaclass := core.StandardClassClass
	abstractp := Nil
	for _, classOpt := range classOpts {
		var err core.Instance
		switch {
		case core.DeepEqual(classOpt.(*core.Cons).Car, core.NewSymbol(":METACLASS")):
			if metaclass, err = Class(e, classOpt.(core.List).Nth(1)); err != nil {
				return nil, err
			}
		case core.DeepEqual(classOpt.(*core.Cons).Car, core.NewSymbol(":ABSTRACTP")):
			if abstractp, err = Eval(e, classOpt.(core.List).Nth(1)); err != nil {
				return nil, err
			}
		}
	}
	classObject := core.NewStandardClass(className, supers, slots, initforms, initargs, metaclass, abstractp)
	e.Class[:1].Define(className, classObject)
	for _, slotSpec := range slotSpecs.(core.List).Slice() {
		if core.InstanceOf(core.SymbolClass, slotSpec) {
			continue
		}
		slotName := slotSpec.(*core.Cons).Car
		slotOpts := slotSpec.(*core.Cons).Cdr.(core.List).Slice()
		var readerFunctionName, writerFunctionName, boundpFunctionName core.Instance
		for i := 0; i < len(slotOpts); i += 2 {
			switch {
			case core.DeepEqual(slotOpts[i], core.NewSymbol(":READER")):
				readerFunctionName = slotOpts[i+1]
			case core.DeepEqual(slotOpts[i], core.NewSymbol(":WRITER")):
				writerFunctionName = slotOpts[i+1]
			case core.DeepEqual(slotOpts[i], core.NewSymbol(":ACCESSOR")):
				readerFunctionName = slotOpts[i+1]
				writerFunctionName = core.NewSymbol(fmt.Sprintf("(SETF %v)", slotOpts[i+1]))
			case core.DeepEqual(slotOpts[i], core.NewSymbol(":BOUNDP")):
				boundpFunctionName = slotOpts[i+1]
			}
		}
		if readerFunctionName != nil {
			lambdaList, err := List(e, core.NewSymbol("INSTANCE"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(readerFunctionName); !ok || !core.InstanceOf(core.GenericFunctionClass, g) {
				Defgeneric(e, readerFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(readerFunctionName)
			fun.(*core.GenericFunction).AddMethod(nil, lambdaList, []core.Class{classObject}, core.NewFunction(readerFunctionName, func(e core.Environment, object core.Instance) (core.Instance, core.Instance) {
				slot, ok := object.(core.BasicInstance).GetSlotValue(slotName, classObject)
				if ok {
					return slot, nil
				}
				return Nil, nil // TODO: shoud throw an error.
			}))
		}
		if writerFunctionName != nil {
			lambdaList, err := List(e, core.NewSymbol("Y"), core.NewSymbol("X"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(writerFunctionName); !ok || !core.InstanceOf(core.GenericFunctionClass, g) {
				Defgeneric(e, writerFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(writerFunctionName)
			fun.(*core.GenericFunction).AddMethod(nil, lambdaList, []core.Class{core.ObjectClass, classObject}, core.NewFunction(writerFunctionName, func(e core.Environment, obj, object core.Instance) (core.Instance, core.Instance) {
				ok := object.(core.BasicInstance).SetSlotValue(obj, slotName, classObject)
				if ok {
					return obj, nil
				}
				return Nil, nil
			}))
		}
		if boundpFunctionName != nil {
			lambdaList, err := List(e, core.NewSymbol("INSTANCE"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(boundpFunctionName); !ok || !core.InstanceOf(core.GenericFunctionClass, g) {
				Defgeneric(e, boundpFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(boundpFunctionName)
			fun.(*core.GenericFunction).AddMethod(nil, lambdaList, []core.Class{classObject}, core.NewFunction(boundpFunctionName, func(e core.Environment, object core.Instance) (core.Instance, core.Instance) {
				_, ok := object.(core.BasicInstance).GetSlotValue(slotName, classObject)
				if ok {
					return T, nil
				}
				return Nil, nil
			}))
		}
	}
	return className, nil
}

func Create(e core.Environment, c core.Instance, i ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.StandardClassClass, c); err != nil {
		return nil, err
	}
	return core.Create(e, c, i...), nil
}

func InitializeObject(e core.Environment, object core.Instance, inits ...core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.StandardObjectClass, object); err != nil {
		return nil, err
	}
	return core.InitializeObject(e, object, inits...), nil
}

func Defmethod(e core.Environment, arguments ...core.Instance) (core.Instance, core.Instance) {
	if len(arguments) < 2 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	name := arguments[0]
	var qualifier core.Instance
	i := 0
	if core.DeepEqual(arguments[1], core.NewSymbol(":AROUND")) || core.DeepEqual(arguments[1], core.NewSymbol(":BEFORE")) || core.DeepEqual(arguments[1], core.NewSymbol(":AFTER")) {
		qualifier = arguments[1]
		i++
	}
	parameterList := []core.Instance{}
	for _, pp := range arguments[i+1].(core.List).Slice() {
		if core.InstanceOf(core.SymbolClass, pp) {
			parameterList = append(parameterList, pp)
		} else {
			parameterList = append(parameterList, pp.(core.List).Nth(0))
		}
	}
	lambdaList, err := List(e, parameterList...)
	if err != nil {
		return nil, err
	}
	classList := []core.Class{}
	for _, pp := range arguments[i+1].(core.List).Slice() {
		if core.DeepEqual(pp, core.NewSymbol(":REST")) && core.DeepEqual(pp, core.NewSymbol("&REST")) {
			break
		}
		if core.InstanceOf(core.SymbolClass, pp) {
			classList = append(classList, core.ObjectClass)
		} else {
			class, ok := e.Class[:1].Get(pp.(core.List).Nth(1))
			if !ok {
				return SignalCondition(e, core.NewUndefinedClass(e, pp.(core.List).Nth(1)), Nil)

			}
			classList = append(classList, class.(core.Class))
		}
	}
	fun, err := newNamedFunction(e, name, lambdaList, arguments[i+2:]...)
	if err != nil {
		return nil, err
	}
	gen, ok := e.Function[:1].Get(name)
	if !ok {
		return SignalCondition(e, core.NewUndefinedFunction(e, name), Nil)
	}
	if !gen.(*core.GenericFunction).AddMethod(qualifier, lambdaList, classList, fun) {
		return SignalCondition(e, core.NewUndefinedFunction(e, name), Nil)
	}
	return name, nil
}

func Defgeneric(e core.Environment, funcSpec, lambdaList core.Instance, optionsOrMethodDescs ...core.Instance) (core.Instance, core.Instance) {
	var methodCombination core.Instance
	genericFunctionClass := core.StandardGenericFunctionClass
	forms := []core.Instance{}
	for _, optionOrMethodDesc := range optionsOrMethodDescs {
		switch {
		case core.DeepEqual(optionOrMethodDesc.(core.List).Nth(0), core.NewSymbol(":METHOD-COMBINATION")):
			methodCombination = optionOrMethodDesc.(core.List).Nth(1)
		case core.DeepEqual(optionOrMethodDesc.(core.List).Nth(0), core.NewSymbol(":GENERIC-FUNCTION-CLASS")):
			class, ok := e.Class[:1].Get(optionOrMethodDesc.(core.List).Nth(1))
			if !ok {
				return SignalCondition(e, core.NewUndefinedClass(e, optionOrMethodDesc.(core.List).Nth(1)), Nil)
			}
			genericFunctionClass = class.(core.Class)
		case core.DeepEqual(optionOrMethodDesc.(core.List).Nth(0), core.NewSymbol(":METHOD")):
			forms = append(forms, core.NewCons(core.NewSymbol("DEFMETHOD"), optionOrMethodDesc.(core.List).NthCdr(1)))
		}
	}
	e.Function[:1].Define(
		core.NewSymbol(
			fmt.Sprint(funcSpec),
		),
		core.NewGenericFunction(
			funcSpec,
			lambdaList,
			methodCombination,
			genericFunctionClass,
		),
	)
	Progn(e, forms...)
	return funcSpec, nil
}

func GenericFunctionP(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.InstanceOf(core.GenericFunctionClass, obj) {
		return T, nil
	}
	return Nil, nil
}
