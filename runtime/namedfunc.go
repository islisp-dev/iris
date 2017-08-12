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

func newNamedFunction(local, global *environment.Environment, functionName, lambdaList ilos.Instance, forms ...ilos.Instance) ilos.Instance {
	lexical := local
	cdr := lambdaList
	parameters := []ilos.Instance{}
	variadic := false
	for instance.Of(class.Cons, cdr) {
		cadr := instance.UnsafeCar(cdr) // Checked at the top of this loop
		if cadr == instance.New(class.Symbol, ":REST") || cadr == instance.New(class.Symbol, "&REST") {
			variadic = true
		}
		parameters = append(parameters, cadr)
		cdr = instance.UnsafeCdr(cdr) // Checked at the top of this loop
	}
	return instance.New(class.Function, functionName, func(local, global *environment.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
		local.BlockTag = append(lexical.BlockTag, local.BlockTag...)
		local.TagbodyTag = append(lexical.TagbodyTag, local.TagbodyTag...)
		local.CatchTag = append(lexical.CatchTag, local.CatchTag...)
		local.Variable = append(lexical.Variable, local.Variable...)
		local.Function = append(lexical.Function, local.Function...)
		local.Special = append(lexical.Special, local.Special...)
		local.Macro = append(lexical.Macro, local.Macro...)
		local.DynamicVariable = append(lexical.DynamicVariable, local.DynamicVariable...)
		if (variadic && len(parameters)-2 > len(arguments)) || (!variadic && len(parameters) != len(arguments)) {
			v := Nil
			for i := len(arguments) - 1; i >= 0; i-- {
				v = instance.New(class.Cons, arguments[i], v)
			}
			return nil, instance.New(class.ProgramError)
		}
		for idx := range parameters {
			key := parameters[idx]
			if key == instance.New(class.Symbol, ":REST") || key == instance.New(class.Symbol, "&REST") {
				key := parameters[idx+1]
				value := Nil
				for i := len(arguments) - 1; i >= idx; i-- {
					value = instance.New(class.Cons, arguments[i], value)
				}
				if local.Variable.Define(key, value) {
					return nil, instance.New(class.ProgramError)
				}
				break
			}
			value := arguments[idx]
			if local.Variable.Define(key, value) {
				return nil, instance.New(class.ProgramError)
			}
		}
		ret := Nil
		var err ilos.Instance
		for _, form := range forms {
			ret, err = Eval(local, global, form)
			if err != nil {
				return nil, err
			}
		}
		return ret, nil
	})
}
