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

func Class(local, global environment.Environment, className ilos.Instance) (ilos.Class, ilos.Instance) {
	if v, ok := global.Class.Get(className); ok {
		return v.(ilos.Class), nil
	}
	return nil, nil
}

func Defclass(local, global environment.Environment, className, scNames, slotSpecs ilos.Instance, classOpts ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Symbol, className); err != nil {
		return nil, err
	}
	if err := ensure(class.List, scNames, slotSpecs); err != nil {
		return nil, err
	}
	supers := []ilos.Class{}
	for _, scName := range scNames.(instance.List).Slice() {
		super, err := Class(local, global, scName)
		if err != nil {
			return nil, err
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
				initforms[slotName] = slotOpts[i+1]
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
			if metaclass, err = Class(local, global, classOpt.(*instance.Cons).Cdr.(*instance.Cons).Car); err != nil {
				return nil, err
			}
		case instance.NewSymbol(":ABSTRACTP"):
			if abstractp, err = Eval(local, global, classOpt.(*instance.Cons).Cdr.(*instance.Cons).Car); err != nil {
				return nil, err
			}
		}
	}
	global.Class.Define(className, instance.NewStandardClass(className, supers, slots, initforms, initargs, metaclass, abstractp))
	for _, slotSpec := range slotSpecs.(instance.List).Slice() {
		if ilos.InstanceOf(class.Symbol, slotSpec) {
			continue
		}
		// slotName := slotSpec.(*instance.Cons).Car
		slotOpts := slotSpec.(*instance.Cons).Cdr.(instance.List).Slice()
		for i := 0; i < len(slotOpts); i += 2 {
			switch slotOpts[i] {
			// Add to generic functions
			case instance.NewSymbol(":READER"):
			case instance.NewSymbol(":WRITER"):
			case instance.NewSymbol(":ACCESSOR"):
			case instance.NewSymbol(":BOUNDP"):
			}
		}
	}
	return className, nil
}
