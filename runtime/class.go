// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"fmt"
	"reflect"

	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func ClassOf(e env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	return obj.Class(), nil
}

func Instancep(e env.Environment, obj ilos.Instance, class ilos.Class) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(class, obj) {
		return T, nil
	}
	return Nil, nil
}

func Subclassp(e env.Environment, class1, class2 ilos.Class) (ilos.Instance, ilos.Instance) {
	if ilos.SubclassOf(class1, class2) {
		return T, nil
	}
	return Nil, nil
}

func Class(e env.Environment, className ilos.Instance) (ilos.Class, ilos.Instance) {
	if v, ok := e.Class[:1].Get(className); ok {
		return v.(ilos.Class), nil
	}
	return nil, instance.NewUndefinedClass(className)
}

func checkSuperClass(a, b ilos.Class) bool {
	if reflect.DeepEqual(a, class.StandardObject) || reflect.DeepEqual(b, class.StandardObject) {
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

func Defclass(e env.Environment, className, scNames, slotSpecs ilos.Instance, classOpts ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Symbol, className); err != nil {
		return nil, err
	}
	if err := ensure(class.List, scNames, slotSpecs); err != nil {
		return nil, err
	}
	supers := []ilos.Class{class.StandardObject}
	for _, scName := range scNames.(instance.List).Slice() {
		super, err := Class(e, scName)
		if err != nil {
			return nil, err
		}
		for _, before := range supers {
			if checkSuperClass(before, super) {
				return nil, instance.NewArityError()
			}
		}
		supers = append(supers, super.(ilos.Class))
	}
	slots := []ilos.Instance{}
	initforms := map[ilos.Instance]ilos.Instance{}
	initargs := map[ilos.Instance]ilos.Instance{}
	for _, slotSpec := range slotSpecs.(instance.List).Slice() {
		if ilos.InstanceOf(class.Symbol, slotSpec) {
			slotName := slotSpec
			slots = append(slots, slotName)
			continue
		}
		slotName := slotSpec.(*instance.Cons).Car
		slots = append(slots, slotName)
		slotOpts := slotSpec.(*instance.Cons).Cdr.(instance.List).Slice()
		for i := 0; i < len(slotOpts); i += 2 {
			switch slotOpts[i] {
			case instance.NewSymbol(":INITFORM"):
				closure, err := newNamedFunction(e, instance.NewSymbol("CLOSURE"), Nil, slotOpts[i+1])
				if err != nil {
					return nil, err
				}
				initforms[slotName] = closure
			case instance.NewSymbol(":INITARG"):
				initargs[slotOpts[i+1]] = slotName
			}
		}
	}
	metaclass := class.StandardClass
	abstractp := Nil
	for _, classOpt := range classOpts {
		var err ilos.Instance
		switch classOpt.(*instance.Cons).Car {
		case instance.NewSymbol(":METACLASS"):
			if metaclass, err = Class(e, classOpt.(instance.List).Nth(1)); err != nil {
				return nil, err
			}
		case instance.NewSymbol(":ABSTRACTP"):
			if abstractp, err = Eval(e, classOpt.(instance.List).Nth(1)); err != nil {
				return nil, err
			}
		}
	}
	classObject := instance.NewStandardClass(className, supers, slots, initforms, initargs, metaclass, abstractp)
	e.Class[:1].Define(className, classObject)
	for _, slotSpec := range slotSpecs.(instance.List).Slice() {
		if ilos.InstanceOf(class.Symbol, slotSpec) {
			continue
		}
		slotName := slotSpec.(*instance.Cons).Car
		slotOpts := slotSpec.(*instance.Cons).Cdr.(instance.List).Slice()
		var readerFunctionName, writerFunctionName, boundpFunctionName ilos.Instance
		for i := 0; i < len(slotOpts); i += 2 {
			switch slotOpts[i] {
			case instance.NewSymbol(":READER"):
				readerFunctionName = slotOpts[i+1]
			case instance.NewSymbol(":WRITER"):
				writerFunctionName = slotOpts[i+1]
			case instance.NewSymbol(":ACCESSOR"):
				readerFunctionName = slotOpts[i+1]
				writerFunctionName = instance.NewSymbol(fmt.Sprintf("(SETF %v)", slotOpts[i+1]))
			case instance.NewSymbol(":BOUNDP"):
				boundpFunctionName = slotOpts[i+1]
			}
		}
		if readerFunctionName != nil {
			lambdaList, err := List(e, instance.NewSymbol("INSTANCE"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(readerFunctionName); !ok || !ilos.InstanceOf(class.GenericFunction, g) {
				Defgeneric(e, readerFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(readerFunctionName)
			fun.(*instance.GenericFunction).AddMethod(nil, lambdaList, []ilos.Class{classObject}, instance.NewFunction(readerFunctionName, func(e env.Environment, object ilos.Instance) (ilos.Instance, ilos.Instance) {
				slot, ok := object.(instance.Instance).GetSlotValue(slotName, classObject)
				if ok {
					return slot, nil
				}
				return Nil, nil // TODO: shoud throw an error.
			}))
		}
		if writerFunctionName != nil {
			lambdaList, err := List(e, instance.NewSymbol("Y"), instance.NewSymbol("X"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(writerFunctionName); !ok || !ilos.InstanceOf(class.GenericFunction, g) {
				Defgeneric(e, writerFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(writerFunctionName)
			fun.(*instance.GenericFunction).AddMethod(nil, lambdaList, []ilos.Class{class.Object, classObject}, instance.NewFunction(writerFunctionName, func(e env.Environment, obj, object ilos.Instance) (ilos.Instance, ilos.Instance) {
				ok := object.(instance.Instance).SetSlotValue(obj, slotName, classObject)
				if ok {
					return obj, nil
				}
				return Nil, nil
			}))
		}
		if boundpFunctionName != nil {
			lambdaList, err := List(e, instance.NewSymbol("INSTANCE"))
			if err != nil {
				return nil, err
			}
			if g, ok := e.Function.Get(boundpFunctionName); !ok || !ilos.InstanceOf(class.GenericFunction, g) {
				Defgeneric(e, boundpFunctionName, lambdaList)
			}
			fun, _ := e.Function.Get(boundpFunctionName)
			fun.(*instance.GenericFunction).AddMethod(nil, lambdaList, []ilos.Class{classObject}, instance.NewFunction(boundpFunctionName, func(e env.Environment, object ilos.Instance) (ilos.Instance, ilos.Instance) {
				_, ok := object.(instance.Instance).GetSlotValue(slotName, classObject)
				if ok {
					return T, nil
				}
				return Nil, nil
			}))
		}
	}
	return className, nil
}

func Create(e env.Environment, c ilos.Instance, i ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.StandardClass, c); err != nil {
		return nil, err
	}
	return instance.Create(e, c, i...), nil
}

func InitializeObject(e env.Environment, object ilos.Instance, inits ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.StandardObject, object); err != nil {
		return nil, err
	}
	return instance.InitializeObject(e, object, inits...), nil
}
