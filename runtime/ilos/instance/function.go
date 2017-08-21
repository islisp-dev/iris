// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"fmt"
	"reflect"
	"sort"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
)

type Applicable interface {
	Apply(environment.Environment, environment.Environment, ...ilos.Instance) (ilos.Instance, ilos.Instance)
}

type Function struct {
	name     ilos.Instance
	function interface{}
}

func NewFunction(name ilos.Instance, function interface{}) ilos.Instance {
	return Function{name, function}
}

func (Function) Class() ilos.Class {
	return FunctionClass
}

func (Function) GetSlotValue(_ ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (Function) SetSlotValue(_, _ ilos.Instance, _ ilos.Class) bool {
	return false
}

func (f Function) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

func (f Function) Apply(local, global environment.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	fv := reflect.ValueOf(f.function)
	ft := reflect.TypeOf(f.function)
	argv := []reflect.Value{reflect.ValueOf(local), reflect.ValueOf(global)}
	for _, cadr := range arguments {
		argv = append(argv, reflect.ValueOf(cadr))
	}
	if ft.NumIn() != len(argv) && (!ft.IsVariadic() || ft.NumIn()-2 >= len(argv)) {
		return nil, NewArityError()
	}
	rets := fv.Call(argv)
	a, _ := rets[0].Interface().(ilos.Instance)
	b, _ := rets[1].Interface().(ilos.Instance)
	return a, b

}

type method struct {
	qualifier ilos.Instance
	classList []ilos.Class
	function  Function
}

type GenericFunction struct {
	funcSpec             ilos.Instance
	lambdaList           ilos.Instance
	methodCombination    ilos.Instance
	genericFunctionClass ilos.Class
	methods              []method
}

func NewGenericFunction(funcSpec, lambdaList, methodCombination ilos.Instance, genericFunctionClass ilos.Class) ilos.Instance {
	return &GenericFunction{funcSpec, lambdaList, methodCombination, genericFunctionClass, []method{}}
}

func (f *GenericFunction) AddMethod(qualifier, lambdaList ilos.Instance, classList []ilos.Class, function ilos.Instance) bool {
	if f.lambdaList.(List).Length() != lambdaList.(List).Length() {
		return false
	}
	for i, param := range f.lambdaList.(List).Slice() {
		if param == NewSymbol(":REST") || param == NewSymbol("&REST") {
			if lambdaList.(List).Nth(i) != NewSymbol(":REST") && lambdaList.(List).Nth(i) != NewSymbol("&REST") {
				return false
			}
		}
	}
	for _, method := range f.methods {
		if method.qualifier == qualifier {
			method.function = function.(Function)
			return true
		}
	}
	f.methods = append(f.methods, method{qualifier, classList, function.(Function)})
	return true
}

func (f *GenericFunction) Class() ilos.Class {
	return f.genericFunctionClass
}

func (f *GenericFunction) GetSlotValue(_ ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (f *GenericFunction) SetSlotValue(_, _ ilos.Instance, _ ilos.Class) bool {
	return false
}

func (f *GenericFunction) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

func (f *GenericFunction) Apply(local, global environment.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	parameters := f.lambdaList.(List).Slice()
	variadic := false
	{
		test := func(i int) bool { return parameters[i] == NewSymbol(":REST") || parameters[i] == NewSymbol("&REST") }
		if sort.Search(len(parameters), test) < len(parameters) {
			variadic = true
		}
	}
	if (variadic && len(parameters)-2 > len(arguments)) || (!variadic && len(parameters) != len(arguments)) {
		return nil, NewArityError()
	}
	methods := []method{}
	for _, method := range f.methods {
		matched := true
		for i, c := range method.classList {
			if !ilos.InstanceOf(c, arguments[i]) {
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
			if ilos.SubclassOf(methods[a].classList[i], methods[b].classList[i]) {
				return false
			}
			if ilos.SubclassOf(methods[b].classList[i], methods[a].classList[i]) {
				return true
			}
		}
		t := map[ilos.Instance]int{around: 4, before: 3, nil: 2, after: 1}
		return t[methods[a].qualifier] > t[methods[b].qualifier]
	})

	nextMethodPisNil := NewFunction(NewSymbol("NEXT-METHOD-P"), func(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
		return Nil, nil
	})
	nextMethodPisT := NewFunction(NewSymbol("NEXT-METHOD-P"), func(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
		return T, nil
	})
	if f.methodCombination == NewSymbol("NIL") {
		var callNextMethod func(local, global environment.Environment) (ilos.Instance, ilos.Instance) // To Recursive
		callNextMethod = func(local, global environment.Environment) (ilos.Instance, ilos.Instance) { // CALL-NEXT-METHOD
			depth, _ := local.DynamicVariable.Get(NewSymbol("IRIS/DEPTH"))           // Get previous depth
			index := int(depth.(Integer)) + 1                                        // Get index of next method
			local.DynamicVariable.Define(NewSymbol("IRIS/DEPTH"), NewInteger(index)) // Set current depth
			// If Generic Function has no next-mehtods,  NEXT-METHOD-P local function returns nil
			local.Function.Define(NewSymbol("NEXT-METHOD-P"), NewFunction(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil))
			if int(depth.(Integer))+1 < len(methods) { // If Generic Function has next method, set these functionss
				local.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
				local.Function.Define(NewSymbol("NEXT-METHOD-P"), NewFunction(NewSymbol("NEXT-METHOD-P"), nextMethodPisT))
			}
			return methods[index].function.Apply(local, global, arguments...) // Call next method
		}
		local.DynamicVariable.Define(NewSymbol("IRIS/DEPTH"), NewInteger(0)) // Set current depth
		// If Generic Function has no next-mehtods,  NEXT-METHOD-P local function returns nil
		local.Function.Define(NewSymbol("NEXT-METHOD-P"), NewFunction(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil))
		if 1 < len(methods) { // If Generic Function has next method, set these functionss
			local.Function.Define(NewSymbol("NEXT-METHOD-P"), NewFunction(NewSymbol("NEXT-METHOD-P"), nextMethodPisT))
			local.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
		}
		return methods[0].function.Apply(local, global, arguments...) //Call first of method
	}
	// if f.methodCombination == NewSymbol("STANDARD")
	{
		test := func(i int) bool { return methods[i].qualifier == around }
		width := len(methods)
		if index := sort.Search(width, test); index < width { // if has :around methods
			// This callNextMethod is called in :around methods
			var callNextMethod func(local, global environment.Environment) (ilos.Instance, ilos.Instance)
			callNextMethod = func(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
				depth, _ := local.DynamicVariable.Get(NewSymbol("IRIS/DEPTH")) // Get previous depth
				for index, method := range methods[:int(depth.(Integer))+1] {
					if method.qualifier == around { // If have :around method
						local.DynamicVariable.Define(NewSymbol("IRIS/DEPTH"), NewInteger(index)) // Set Current depth
						// If Generic Function has no next-mehtods,  NEXT-METHOD-P local function returns nil
						local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
						{ // If Generic Function has next method, set these functionss
							width := len(methods) - index - 1
							test := func(i int) bool { return methods[index+i+1].qualifier == nil || methods[index+i+1].qualifier == around }
							if sort.Search(width, test) < width {
								local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
								local.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
							}
						}
						return methods[int(depth.(Integer))].function.Apply(local, global, arguments...) // Call next method
					}
				}
				// If has no :around method then,
				// Do All :before mehtods
				for _, method := range methods {
					if method.qualifier == before {
						if _, err := method.function.Apply(local, global, arguments...); err != nil {
							return nil, err
						}
					}
				}
				// Do the first of primary methods
				// this callNextMethod is called in primary methods
				var callNextMethod func(local, global environment.Environment) (ilos.Instance, ilos.Instance)
				callNextMethod = func(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
					depth, _ := local.DynamicVariable.Get(NewSymbol("IRIS/DEPTH")) // Get previous depth
					index := int(depth.(Integer))                                  // Convert depth to integer
					{
						width := len(methods) - index - 1
						test := func(i int) bool { return methods[index+i+1].qualifier == nil }
						index = sort.Search(width, test)                                         // Get index of next mehotd
						local.DynamicVariable.Define(NewSymbol("IRIS/DEPTH"), NewInteger(index)) // Set current depth
					}
					// If Generic Function has no next-mehtods,  NEXT-METHOD-P local function returns nil
					local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
					{ // If Generic Function has next method, set these functionss
						width := len(methods) - index - 1
						test := func(i int) bool { return methods[index+i+1].qualifier == nil }
						if sort.Search(width, test) < width {
							local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
							local.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
						}
					}
					return methods[index].function.Apply(local, global, arguments...) // Call next method
				} // callNextMethod ends here
				index := 0 // index of the first primary method
				{          // index != 0 is always true because this function has :around methods
					width := len(methods) - index - 1
					test := func(i int) bool { return methods[index+i+1].qualifier == nil }
					index = sort.Search(width, test)
					local.DynamicVariable.Define(NewSymbol("IRIS/DEPTH"), NewInteger(index))
				}
				// If Generic Function has no next-mehtods,  NEXT-METHOD-P local function returns nil
				local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
				{ // If Generic Function has next method, set these functionss
					test := func(i int) bool { return methods[index+i+1].qualifier == nil }
					width := len(methods) - index - 1
					if sort.Search(width, test) < width {
						local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
						local.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
					}
				}
				// Do primary methods
				ret, err := methods[index].function.Apply(local, global, arguments...)
				if err != nil {
					return nil, err
				}
				// Do all :after methods
				for i := len(methods) - 1; i >= 0; i-- {
					if methods[i].qualifier == after {
						if _, err := methods[i].function.Apply(local, global, arguments...); err != nil {
							return nil, err
						}
					}
				}
				return ret, err
			}
			local.DynamicVariable.Define(NewSymbol("IRIS/DEPTH"), NewInteger(index)) // Set Current depth
			// If Generic Function has no next-mehtods,  NEXT-METHOD-P local function returns nil
			local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
			{ // If Generic Function has next method, set these functionss
				test := func(i int) bool { return methods[index+i+1].qualifier == nil }
				width := len(methods) - index - 1
				if sort.Search(width, test) < width {
					local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
					local.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
				}
			}
			return methods[index].function.Apply(local, global, arguments...)
		}
	}
	{ // Function has no :around methods
		// This callNextMethod is called in primary methods
		var callNextMethod func(local, global environment.Environment) (ilos.Instance, ilos.Instance)
		callNextMethod = func(local, global environment.Environment) (ilos.Instance, ilos.Instance) {
			depth, _ := local.DynamicVariable.Get(NewSymbol("IRIS/DEPTH")) // Get previous depth
			index := int(depth.(Integer))                                  // Convert depth to integer
			{
				test := func(i int) bool { return methods[index+i+1].qualifier == nil }
				width := len(methods) - index - 1
				index = sort.Search(width, test)
			}
			local.DynamicVariable.Define(NewSymbol("IRIS/DEPTH"), NewInteger(index)) // Set Current depth
			// If Generic Function has no next-mehtods,  NEXT-METHOD-P local function returns nil
			local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
			{ // If Generic Function has next method, set these functionss
				test := func(i int) bool { return methods[index+i+1].qualifier == nil }
				width := len(methods) - index - 1
				if sort.Search(width, test) < width {
					local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
					local.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
				}
			}
			return methods[int(depth.(Integer))].function.Apply(local, global, arguments...)
		} // callNextMethod ends here
		// Do All :before mehtods
		for _, method := range methods {
			if method.qualifier == before {
				if _, err := method.function.Apply(local, global, arguments...); err != nil {
					return nil, err
				}
			}
		}
		index := 0 // index of the first primary method
		{
			test := func(i int) bool { return methods[i].qualifier == nil }
			width := len(methods)
			index := sort.Search(width, test)
			local.DynamicVariable.Define(NewSymbol("IRIS/DEPTH"), NewInteger(index))
			if index == len(methods) {
				return nil, NewUndefinedFunction(f.funcSpec)
			}
		}
		local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisNil)
		{ // If Generic Function has next method, set these functions
			test := func(i int) bool { return methods[index+i+1].qualifier == nil }
			width := len(methods) - index - 1
			if sort.Search(width, test) < width {
				local.Function.Define(NewSymbol("NEXT-METHOD-P"), nextMethodPisT)
				local.Function.Define(NewSymbol("CALL-NEXT-METHOD"), NewFunction(NewSymbol("CALL-NEXT-METHOD"), callNextMethod))
			}
		}
		ret, err := methods[index].function.Apply(local, global, arguments...)
		// Do all :after methods
		for i := len(methods) - 1; i >= 0; i-- {
			if methods[i].qualifier == after {
				if _, err := methods[i].function.Apply(local, global, arguments...); err != nil {
					return nil, err
				}
			}
		}
		return ret, err
	}
}
