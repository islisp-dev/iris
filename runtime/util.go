// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"reflect"
	"regexp"
	"runtime"
	"strings"

	"github.com/ta2gch/iris/runtime/env"

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

func convFloat64(e env.Environment, x ilos.Instance) (float64, bool, ilos.Instance) {
	switch {
	case ilos.InstanceOf(class.Integer, x):
		return float64(x.(instance.Integer)), false, nil
	case ilos.InstanceOf(class.Float, x):
		return float64(x.(instance.Float)), true, nil
	default:
		condition := instance.Create(e, class.Number,
			instance.NewSymbol("OBJECT"), x,
			instance.NewSymbol("EXPECTED-CLASS"), class.Number)
		_, err := SignalCondition(e, condition, Nil)
		return 0.0, false, err
	}
}

func readFromString(s string) ilos.Instance {
	ret, _ := parser.Parse(tokenizer.Tokenize(strings.NewReader(s)))
	return ret
}
func evalString(e env.Environment, s string) ilos.Instance {
	ret, _ := Eval(e, readFromString(s))
	return ret
}

func ensure(e env.Environment, c ilos.Class, i ...ilos.Instance) ilos.Instance {
	for _, o := range i {
		if !ilos.InstanceOf(c, o) {
			condition := instance.Create(e, class.DomainError,
				instance.NewSymbol("OBJECT"), o,
				instance.NewSymbol("EXPECTED-CLASS"), c)
			_, err := SignalCondition(e, condition, Nil)
			return err
		}
	}
	return nil
}

var uidsrc = 0

func genUID() ilos.Instance {
	uidsrc++
	return instance.NewInteger(uidsrc)
}

func func2symbol(function interface{}) ilos.Instance {
	name := runtime.FuncForPC(reflect.ValueOf(function).Pointer()).Name()
	name = regexp.MustCompile(`.*\.`).ReplaceAllString(name, "")
	name = regexp.MustCompile(`(.)([A-Z])`).ReplaceAllString(name, "$1-$2")
	name = strings.ToUpper(name)
	return instance.NewSymbol(name)
}
