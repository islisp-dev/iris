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

func checkLambdaList(lambdaList ilos.Instance) ilos.Instance {
	cdr, _, err := convSlice(lambdaList)
	if err != nil {
		return err
	}
	for i, cadr := range cdr {
		if !instance.Of(class.Symbol, cadr) {
			return instance.New(class.ProgramError)
		}
		if cadr == instance.New(class.Symbol, ":REST") || cadr == instance.New(class.Symbol, "&REST") {
			if len(cdr) != i+2 {
				return instance.New(class.ProgramError)
			}
		}
	}
	return nil
}

func newNamedFunction(local, global *environment.Environment, functionName, lambdaList ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	lexical := local
	if err := checkLambdaList(lambdaList); err != nil {
		return nil, err
	}
	cdr, _, _ := convSlice(lambdaList)
	parameters := []ilos.Instance{}
	variadic := false
	for _, cadr := range cdr {
		if cadr == instance.New(class.Symbol, ":REST") || cadr == instance.New(class.Symbol, "&REST") {
			variadic = true
		}
		parameters = append(parameters, cadr)
	}
	return instance.New(class.Function, functionName, func(local, global *environment.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
		local.Merge(lexical)
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
				if !local.Variable.Define(key, value) {
					return nil, instance.New(class.ProgramError)
				}
				break
			}
			value := arguments[idx]
			if !local.Variable.Define(key, value) {
				return nil, instance.New(class.ProgramError)
			}
		}
		return Progn(local, global, forms...)
	}), nil
}
