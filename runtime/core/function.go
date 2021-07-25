// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

import (
	"fmt"
	"reflect"
	"sort"
)

type Applicable interface {
	Apply(Environment, ...Instance) (Instance, Instance)
}

type Function struct {
	name     Instance
	function interface{}
}

func NewFunction(name Instance, function interface{}) Instance {
	return Function{name, function}
}

func (Function) Class() Class {
	return FunctionClass
}

func (f Function) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

func (f Function) Apply(e Environment, arguments ...Instance) (Instance, Instance) {
	for _, arg := range arguments {
		if arg == nil {
			return SignalCondition(e, NewDomainError(e, arg, ObjectClass), Nil)
		}
	}
	fv := reflect.ValueOf(f.function)
	ft := reflect.TypeOf(f.function)
	argv := []reflect.Value{reflect.ValueOf(e)}
	for _, cadr := range arguments {
		argv = append(argv, reflect.ValueOf(cadr))
	}
	if ft.NumIn() != len(argv) && (!ft.IsVariadic() || ft.NumIn()-2 >= len(argv)) {
		return SignalCondition(e, NewArityError(e), Nil)
	}
	rets := fv.Call(argv)
	a, _ := rets[0].Interface().(Instance)
	b, _ := rets[1].Interface().(Instance)
	return a, b

}

type method struct {
	qualifier Instance
	classList []Class
	function  Function
}

type GenericFunction struct {
	funcSpec             Instance
	lambdaList           Instance
	methodCombination    Instance
	genericFunctionClass Class
	methods              []method
}

func NewGenericFunction(funcSpec, lambdaList, methodCombination Instance, genericFunctionClass Class) Instance {
	return &GenericFunction{funcSpec, lambdaList, methodCombination, genericFunctionClass, []method{}}
}

func (f *GenericFunction) AddMethod(qualifier, lambdaList Instance, classList []Class, function Instance) bool {
	if f.lambdaList.(List).Length() != lambdaList.(List).Length() {
		return false
	}
	for i, param := range f.lambdaList.(List).Slice() {
		if DeepEqual(param, NewSymbol(":REST")) || DeepEqual(param, NewSymbol("&REST")) {
			if lambdaList.(List).Nth(i) != NewSymbol(":REST") && lambdaList.(List).Nth(i) != NewSymbol("&REST") {
				return false
			}
		}
	}
	for i := range f.methods {
		if DeepEqual(f.methods[i].qualifier, qualifier) && DeepEqual(f.methods[i].classList, classList) {
			f.methods[i].function = function.(Function)
			return true
		}
	}
	f.methods = append(f.methods, method{qualifier, classList, function.(Function)})
	return true
}

func (f *GenericFunction) Class() Class {
	return f.genericFunctionClass
}

func (f *GenericFunction) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

func (f *GenericFunction) Apply(e Environment, arguments ...Instance) (Instance, Instance) {
	parameters := f.lambdaList.(List).Slice()
	variadic := false
	{
		test := func(i int) bool {
			return DeepEqual(parameters[i], NewSymbol(":REST")) || DeepEqual(parameters[i], NewSymbol("&REST"))
		}
		if sort.Search(len(parameters), test) < len(parameters) {
			variadic = true
		}
	}
	if (variadic && len(parameters)-2 > len(arguments)) || (!variadic && len(parameters) != len(arguments)) {
		return SignalCondition(e, NewArityError(e), Nil)
	}
	methods := []method{}
	for _, method := range f.methods {
		matched := true
		for i, c := range method.classList {
			if !InstanceOf(c, arguments[i]) {
				matched = false
				break
			}
		}
		if matched {
			methods = append(methods, method)
		}
	}
	before := NewSymbol(":BEFORE")
	around := NewSymbol(":AROUND")
	after := NewSymbol(":AFTER")
	sort.Slice(methods, func(a, b int) bool {
		for i := range methods[a].classList {
			if SubclassOf(methods[a].classList[i], methods[b].classList[i]) {
				return false
			}
			if SubclassOf(methods[b].classList[i], methods[a].classList[i]) {
				return true
			}
		}
		t := map[Instance]int{around: 4, before: 3, nil: 2, after: 1}
		return t[methods[a].qualifier] > t[methods[b].qualifier]
	})

	nextMethodPisNil := NewFunction(NewSymbol("NEXT-METHOD-P"), func(e Environment) (Instance, Instance) {
		return Nil, nil
	})
	nextMethodPisT := NewFunction(NewSymbol("NEXT-METHOD-P"), func(e Environment) (Instance, Instance) {
		return T, nil
	})
	if DeepEqual(f.methodCombination, Nil) {
		var callNextMethod func(e Environment) (Instance, Instance) // To Recursive
		callNextMethod = func(e Environment) (Instance, Instance) { // CALL-NEXT-METHOD
			depth, _ := e.DynamicVariable.Get(NewSymbol("IRIS.DEPTH"))           // Get previous depth
			index := int(depth.(Integer)) + 1                                    // Get index of next method
			e.DynamicVariable.Define(NewSymbol("IRIS.DEPTH"), NewInteger(index)) // Set current depth
			// If Generic Function has no next-mehtods,  NEXT-METHOD-P e function returns nil
			e.Function.Define(NewSymbol("NEXT-METHOD-P"), NewFunction(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil))
			if int(depth.(Integer))+1 < len(methods) { // If Generic Function has next method, set these functionss
				e.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
				e.Function.Define(NewSymbol("NEXT-METHOD-P"), NewFunction(NewSymbol("NEXT-METHOD-P"), nextMethodPisT))
			}
			return methods[index].function.Apply(e, arguments...) // Call next method
		}
		e.DynamicVariable.Define(NewSymbol("IRIS.DEPTH"), NewInteger(0)) // Set current depth
		// If Generic Function has no next-mehtods,  NEXT-METHOD-P e function returns nil
		e.Function.Define(NewSymbol("NEXT-METHOD-P"), NewFunction(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil))
		if 1 < len(methods) { // If Generic Function has next method, set these functionss
			e.Function.Define(NewSymbol("NEXT-METHOD-P"), NewFunction(NewSymbol("NEXT-METHOD-P"), nextMethodPisT))
			e.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
		}
		return methods[0].function.Apply(e, arguments...) //Call first of method
	}
	// if DeepEqual(f.methodCombination, NewSymbol("STANDARD"))
	{
		test := func(i int) bool { return DeepEqual(methods[i].qualifier, around) }
		width := len(methods)
		if index := sort.Search(width, test); index < width { // if has :around methods
			// This callNextMethod is called in :around methods
			var callNextMethod func(e Environment) (Instance, Instance)
			callNextMethod = func(e Environment) (Instance, Instance) {
				depth, _ := e.DynamicVariable.Get(NewSymbol("IRIS.DEPTH")) // Get previous depth
				for index, method := range methods[:int(depth.(Integer))+1] {
					if DeepEqual(method.qualifier, around) { // If have :around method
						e.DynamicVariable.Define(NewSymbol("IRIS.DEPTH"), NewInteger(index)) // Set Current depth
						// If Generic Function has no next-mehtods,  NEXT-METHOD-P e function returns nil
						e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
						{ // If Generic Function has next method, set these functionss
							width := len(methods) - index - 1
							test := func(i int) bool {
								return DeepEqual(methods[index+i+1].qualifier, nil) || DeepEqual(methods[index+i+1].qualifier, around)
							}
							if sort.Search(width, test) < width {
								e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
								e.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
							}
						}
						return methods[int(depth.(Integer))].function.Apply(e, arguments...) // Call next method
					}
				}
				// If has no :around method then,
				// Do All :before mehtods
				for _, method := range methods {
					if DeepEqual(method.qualifier, before) {
						if _, err := method.function.Apply(e, arguments...); err != nil {
							return nil, err
						}
					}
				}
				// Do the first of primary methods
				// this callNextMethod is called in primary methods
				var callNextMethod func(e Environment) (Instance, Instance)
				callNextMethod = func(e Environment) (Instance, Instance) {
					depth, _ := e.DynamicVariable.Get(NewSymbol("IRIS.DEPTH")) // Get previous depth
					index := int(depth.(Integer))                              // Convert depth to integer
					{
						width := len(methods) - index - 1
						test := func(i int) bool { return DeepEqual(methods[index+i+1].qualifier, nil) }
						index = sort.Search(width, test)                                     // Get index of next mehotd
						e.DynamicVariable.Define(NewSymbol("IRIS.DEPTH"), NewInteger(index)) // Set current depth
					}
					// If Generic Function has no next-mehtods,  NEXT-METHOD-P e function returns nil
					e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
					{ // If Generic Function has next method, set these functionss
						width := len(methods) - index - 1
						test := func(i int) bool { return DeepEqual(methods[index+i+1].qualifier, nil) }
						if sort.Search(width, test) < width {
							e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
							e.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
						}
					}
					return methods[index].function.Apply(e, arguments...) // Call next method
				} // callNextMethod ends here
				index := 0 // index of the first primary method
				{          // index != 0 is always true because this function has :around methods
					width := len(methods) - index - 1
					test := func(i int) bool { return DeepEqual(methods[index+i+1].qualifier, nil) }
					index = sort.Search(width, test)
					e.DynamicVariable.Define(NewSymbol("IRIS.DEPTH"), NewInteger(index))
				}
				// If Generic Function has no next-mehtods,  NEXT-METHOD-P e function returns nil
				e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
				{ // If Generic Function has next method, set these functionss
					test := func(i int) bool { return DeepEqual(methods[index+i+1].qualifier, nil) }
					width := len(methods) - index - 1
					if sort.Search(width, test) < width {
						e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
						e.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
					}
				}
				// Do primary methods
				ret, err := methods[index].function.Apply(e, arguments...)
				if err != nil {
					return nil, err
				}
				// Do all :after methods
				for i := len(methods) - 1; i >= 0; i-- {
					if DeepEqual(methods[i].qualifier, after) {
						if _, err := methods[i].function.Apply(e, arguments...); err != nil {
							return nil, err
						}
					}
				}
				return ret, err
			}
			e.DynamicVariable.Define(NewSymbol("IRIS.DEPTH"), NewInteger(index)) // Set Current depth
			// If Generic Function has no next-mehtods,  NEXT-METHOD-P e function returns nil
			e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
			{ // If Generic Function has next method, set these functionss
				test := func(i int) bool { return DeepEqual(methods[index+i+1].qualifier, nil) }
				width := len(methods) - index - 1
				if sort.Search(width, test) < width {
					e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
					e.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
				}
			}
			return methods[index].function.Apply(e, arguments...)
		}
	}
	{ // Function has no :around methods
		// This callNextMethod is called in primary methods
		var callNextMethod func(e Environment) (Instance, Instance)
		callNextMethod = func(e Environment) (Instance, Instance) {
			depth, _ := e.DynamicVariable.Get(NewSymbol("IRIS.DEPTH")) // Get previous depth
			index := int(depth.(Integer))                              // Convert depth to integer
			{
				test := func(i int) bool { return DeepEqual(methods[index+i+1].qualifier, nil) }
				width := len(methods) - index - 1
				index = sort.Search(width, test)
			}
			e.DynamicVariable.Define(NewSymbol("IRIS.DEPTH"), NewInteger(index)) // Set Current depth
			// If Generic Function has no next-mehtods,  NEXT-METHOD-P e function returns nil
			e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
			{ // If Generic Function has next method, set these functionss
				test := func(i int) bool { return DeepEqual(methods[index+i+1].qualifier, nil) }
				width := len(methods) - index - 1
				if sort.Search(width, test) < width {
					e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
					e.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
				}
			}
			return methods[int(depth.(Integer))].function.Apply(e, arguments...)
		} // callNextMethod ends here
		// Do All :before mehtods
		for _, method := range methods {
			if DeepEqual(method.qualifier, before) {
				if _, err := method.function.Apply(e, arguments...); err != nil {
					return nil, err
				}
			}
		}
		index := 0 // index of the first primary method
		{
			test := func(i int) bool { return DeepEqual(methods[i].qualifier, nil) }
			width := len(methods)
			index := sort.Search(width, test)
			e.DynamicVariable.Define(NewSymbol("IRIS.DEPTH"), NewInteger(index))
			if index == len(methods) {
				return SignalCondition(e, NewUndefinedFunction(e, f.funcSpec), Nil)
			}
		}
		e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
		{ // If Generic Function has next method, set these functions
			test := func(i int) bool { return DeepEqual(methods[index+i+1].qualifier, nil) }
			width := len(methods) - index - 1
			if sort.Search(width, test) < width {
				e.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
				e.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
			}
		}
		ret, err := methods[index].function.Apply(e, arguments...)
		// Do all :after methods
		for i := len(methods) - 1; i >= 0; i-- {
			if DeepEqual(methods[i].qualifier, after) {
				if _, err := methods[i].function.Apply(e, arguments...); err != nil {
					return nil, err
				}
			}
		}
		return ret, err
	}
}
