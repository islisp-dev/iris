// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"reflect"
	"regexp"
	"runtime"
	"strings"

	"github.com/ta2gch/iris/runtime/environment"

	"github.com/ta2gch/iris/reader/parser"
	"github.com/ta2gch/iris/reader/tokenizer"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func isProperList(i ilos.Instance) bool {
	if instance.Of(class.Cons, i) {
		return isProperList(i.(*instance.Cons).Cdr) // Checked at the top of this statements
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
		return 0.0, false, instance.NewDomainError(x, class.Number)
	}
}

func readFromString(s string) ilos.Instance {
	e, _ := parser.Parse(tokenizer.New(strings.NewReader(s)))
	return e
}
func evalString(local, global *environment.Environment, s string) ilos.Instance {
	e, _ := Eval(local, global, readFromString(s))
	return e
}

func func2symbol(function interface{}) ilos.Instance {
	name := runtime.FuncForPC(reflect.ValueOf(function).Pointer()).Name()
	name = regexp.MustCompile(`.*\.`).ReplaceAllString(name, "")
	name = regexp.MustCompile(`(.)([A-Z])`).ReplaceAllString(name, "$1-$2")
	name = strings.ToUpper(name)
	return instance.NewSymbol(name)
}

func defspecial(function interface{}) {
	environment.TopLevel.Special.Define(func2symbol(function), instance.NewFunction(func2symbol(function), function))
}

func defmacro(function interface{}) {
	environment.TopLevel.Macro.Define(func2symbol(function), instance.NewFunction(func2symbol(function), function))
}

func defun(function interface{}) {
	environment.TopLevel.Function.Define(func2symbol(function), instance.NewFunction(func2symbol(function), function))
}

func defun2(name string, function interface{}) {
	symbol := instance.NewSymbol(name)
	environment.TopLevel.Function.Define(symbol, instance.NewFunction(symbol, function))
}
func defglobal(name string, value ilos.Instance) {
	symbol := instance.NewSymbol(name)
	environment.TopLevel.Variable.Define(symbol, value)
}
func ensure(c ilos.Class, i ...ilos.Instance) ilos.Instance {
	for _, o := range i {
		if !instance.Of(c, o) {
			return instance.NewDomainError(o, c)
		}
	}
	return nil
}
