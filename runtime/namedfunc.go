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
	if err := ensure(class.List, lambdaList); err != nil {
		return err
	}
	cdr := lambdaList.(instance.List).Slice()
	for i, cadr := range cdr {
		if err := ensure(class.Symbol, cadr); err != nil {
			return err
		}
		if cadr == instance.New(class.Symbol, ":REST") || cadr == instance.New(class.Symbol, "&REST") {
			if len(cdr) != i+2 {
				_, err := ProgramError("ARITY-ERROR")
				return err
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
	parameters := []ilos.Instance{}
	variadic := false
	for _, cadr := range lambdaList.(instance.List).Slice() {
		if cadr == instance.New(class.Symbol, ":REST") || cadr == instance.New(class.Symbol, "&REST") {
			variadic = true
		}
		parameters = append(parameters, cadr)
	}
	return instance.New(class.Function, functionName, func(local, global *environment.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
		local.Merge(lexical)
		if (variadic && len(parameters)-2 > len(arguments)) || (!variadic && len(parameters) != len(arguments)) {
			return ProgramError("ARITY-ERROR")
		}
		for idx := range parameters {
			key := parameters[idx]
			if key == instance.New(class.Symbol, ":REST") || key == instance.New(class.Symbol, "&REST") {
				key := parameters[idx+1]
				value, err := List(nil, nil, arguments[idx:]...)
				if err != nil {
					return nil, err
				}
				if !local.Variable.Define(key, value) {
					return ProgramError("IMMUTABLE-BINDING")
				}
				break
			}
			value := arguments[idx]
			if !local.Variable.Define(key, value) {
				return ProgramError("IMMUTABLE-BINDING")
			}
		}
		return Progn(local, global, forms...)
	}), nil
}
