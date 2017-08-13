// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"strings"

	"github.com/ta2gch/iris/runtime/environment"

	"github.com/ta2gch/iris/reader/parser"
	"github.com/ta2gch/iris/reader/tokenizer"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func convSlice(cons ilos.Instance) ([]ilos.Instance, int, ilos.Instance) {
	is := []ilos.Instance{}
	if isProperList(cons) {
		return nil, 0, instance.New(class.ProgramError)
	}
	for instance.Of(class.Cons, cons) {
		is = append(is, instance.UnsafeCar(cons))
		cons = instance.UnsafeCdr(cons)
	}
	return is, len(is), nil
}

func isProperList(i ilos.Instance) bool {
	if instance.Of(class.Cons, i) {
		return isProperList(instance.UnsafeCdr(i)) // Checked at the top of this statements
	}
	if i == Nil {
		return true
	}
	return false
}

func convFloat64(x ilos.Instance) (float64, bool, ilos.Instance) {
	switch {
	case instance.Of(class.Integer, x):
		return float64(x.(instance.Integer)), false, nil
	case instance.Of(class.Float, x):
		return float64(x.(instance.Float)), true, nil
	default:
		return 0.0, false, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         x,
			"EXPECTED-CLASS": class.Number,
		})
	}
}

func readFromString(s string) ilos.Instance {
	e, _ := parser.Parse(tokenizer.New(strings.NewReader(s)))
	return e
}

func defspecial(name string, macro interface{}) {
	symbol := instance.New(class.Symbol, name)
	environment.TopLevel.Special.Define(symbol, instance.New(class.Function, symbol, macro))
}

func defmacro(name string, macro interface{}) {
	symbol := instance.New(class.Symbol, name)
	environment.TopLevel.Macro.Define(symbol, instance.New(class.Function, symbol, macro))
}

func defun(name string, function interface{}) {
	symbol := instance.New(class.Symbol, name)
	environment.TopLevel.Function.Define(symbol, instance.New(class.Function, symbol, function))
}
func defglobal(name string, value ilos.Instance) {
	symbol := instance.New(class.Symbol, name)
	environment.TopLevel.Variable.Define(symbol, value)
}
func ensure(c ilos.Class, i ...ilos.Instance) ilos.Instance {
	for _, o := range i {
		if !instance.Of(c, o) {
			return instance.New(class.DomainError, map[string]ilos.Instance{
				"OBJECT":         o,
				"EXPECTED-CLASS": c,
			})
		}
	}
	return nil
}
