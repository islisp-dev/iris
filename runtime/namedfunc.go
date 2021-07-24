// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func checkLambdaList(e ilos.Environment, lambdaList ilos.Instance) ilos.Instance {
	if err := ensure(e, instance.ListClass, lambdaList); err != nil {
		return err
	}
	for i, cadr := range lambdaList.(instance.List).Slice() {
		if cadr == instance.NewSymbol(":REST") || cadr == instance.NewSymbol("&REST") {
			if lambdaList.(instance.List).Length() != i+2 {
				_, err := SignalCondition(e, instance.NewArityError(e), Nil)
				return err
			}
		}
	}
	return nil
}

func newNamedFunction(e ilos.Environment, functionName, lambdaList ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	lexical := e
	if err := ensure(e, instance.SymbolClass, functionName); err != nil {
		return nil, err
	}
	if err := checkLambdaList(e, lambdaList); err != nil {
		return nil, err
	}
	parameters := []ilos.Instance{}
	variadic := false
	for _, cadr := range lambdaList.(instance.List).Slice() {
		if cadr == instance.NewSymbol(":REST") || cadr == instance.NewSymbol("&REST") {
			variadic = true
		}
		parameters = append(parameters, cadr)
	}
	return instance.NewFunction(functionName.(instance.Symbol), func(e ilos.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
		e.MergeLexical(lexical)
		if (variadic && len(parameters)-2 > len(arguments)) || (!variadic && len(parameters) != len(arguments)) {
			return SignalCondition(e, instance.NewArityError(e), Nil)
		}
		for idx := range parameters {
			key := parameters[idx]
			if key == instance.NewSymbol(":REST") || key == instance.NewSymbol("&REST") {
				key := parameters[idx+1]
				value, err := List(e, arguments[idx:]...)
				if err != nil {
					return nil, err
				}
				if !e.Variable.Define(key, value) {
					return SignalCondition(e, instance.NewImmutableBinding(e), Nil)
				}
				break
			}
			value := arguments[idx]
			if !e.Variable.Define(key, value) {
				return SignalCondition(e, instance.NewImmutableBinding(e), Nil)
			}
		}
		return Progn(e, forms...)
	}), nil
}
