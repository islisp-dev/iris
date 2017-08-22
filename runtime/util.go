// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"os"
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
	if ilos.InstanceOf(class.Cons, i) {
		return isProperList(i.(*instance.Cons).Cdr) // Checked at the top of this statements
	}
	if i == Nil {
		return true
	}
	return false
}

func convFloat64(x ilos.Instance) (float64, bool, ilos.Instance) {
	switch {
	case ilos.InstanceOf(class.Integer, x):
		return float64(x.(instance.Integer)), false, nil
	case ilos.InstanceOf(class.Float, x):
		return float64(x.(instance.Float)), true, nil
	default:
		return 0.0, false, instance.NewDomainError(x, class.Number)
	}
}

func readFromString(s string) ilos.Instance {
	e, _ := parser.Parse(tokenizer.New(strings.NewReader(s)))
	return e
}
func evalString(local, global environment.Environment, s string) ilos.Instance {
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
	TopLevel.Special.Define(func2symbol(function), instance.NewFunction(func2symbol(function), function))
}

func defmacro(function interface{}) {
	TopLevel.Macro.Define(func2symbol(function), instance.NewFunction(func2symbol(function), function))
}

func defun(function interface{}) {
	TopLevel.Function.Define(func2symbol(function), instance.NewFunction(func2symbol(function), function))
}

func defun2(name string, function interface{}) {
	symbol := instance.NewSymbol(name)
	TopLevel.Function.Define(symbol, instance.NewFunction(symbol, function))
}
func defglobal(name string, value ilos.Instance) {
	symbol := instance.NewSymbol(name)
	TopLevel.Variable.Define(symbol, value)
}
func ensure(c ilos.Class, i ...ilos.Instance) ilos.Instance {
	for _, o := range i {
		if !ilos.InstanceOf(c, o) {
			return instance.NewDomainError(o, c)
		}
	}
	return nil
}

var TopLevel = environment.NewEnvironment(instance.NewStream(os.Stdin, nil), instance.NewStream(nil, os.Stdout), instance.NewStream(nil, os.Stderr))
