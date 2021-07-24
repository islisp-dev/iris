// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/runtime/core"

func checkLambdaList(e core.Environment, lambdaList core.Instance) core.Instance {
	if err := ensure(e, core.ListClass, lambdaList); err != nil {
		return err
	}
	for i, cadr := range lambdaList.(core.List).Slice() {
		if core.DeepEqual(cadr, core.NewSymbol(":REST")) || core.DeepEqual(cadr, core.NewSymbol("&REST")) {
			if lambdaList.(core.List).Length() != i+2 {
				_, err := SignalCondition(e, core.NewArityError(e), Nil)
				return err
			}
		}
	}
	return nil
}

func newNamedFunction(e core.Environment, functionName, lambdaList core.Instance, forms ...core.Instance) (core.Instance, core.Instance) {
	lexical := e
	if err := ensure(e, core.SymbolClass, functionName); err != nil {
		return nil, err
	}
	if err := checkLambdaList(e, lambdaList); err != nil {
		return nil, err
	}
	parameters := []core.Instance{}
	variadic := false
	for _, cadr := range lambdaList.(core.List).Slice() {
		if core.DeepEqual(cadr, core.NewSymbol(":REST")) || core.DeepEqual(cadr, core.NewSymbol("&REST")) {
			variadic = true
		}
		parameters = append(parameters, cadr)
	}
	return core.NewFunction(functionName.(core.Symbol), func(e core.Environment, arguments ...core.Instance) (core.Instance, core.Instance) {
		e.MergeLexical(lexical)
		if (variadic && len(parameters)-2 > len(arguments)) || (!variadic && len(parameters) != len(arguments)) {
			return SignalCondition(e, core.NewArityError(e), Nil)
		}
		for idx := range parameters {
			key := parameters[idx]
			if core.DeepEqual(key, core.NewSymbol(":REST")) || core.DeepEqual(key, core.NewSymbol("&REST")) {
				key := parameters[idx+1]
				value, err := List(e, arguments[idx:]...)
				if err != nil {
					return nil, err
				}
				if !e.Variable.Define(key, value) {
					return SignalCondition(e, core.NewImmutableBinding(e), Nil)
				}
				break
			}
			value := arguments[idx]
			if !e.Variable.Define(key, value) {
				return SignalCondition(e, core.NewImmutableBinding(e), Nil)
			}
		}
		return Progn(e, forms...)
	}), nil
}
