// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"reflect"
	"regexp"
	"runtime"
	"strings"

	"github.com/islisp-dev/iris/reader/parser"
	"github.com/islisp-dev/iris/reader/tokenizer"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func isProperList(i ilos.Instance) bool {
	if ilos.InstanceOf(instance.ConsClass, i) {
		return isProperList(i.(*instance.Cons).Cdr) // Checked at the top of this statements
	}
	if i == Nil {
		return true
	}
	return false
}

func convFloat64(e ilos.Environment, x ilos.Instance) (float64, bool, ilos.Instance) {
	switch {
	case ilos.InstanceOf(instance.IntegerClass, x):
		return float64(x.(instance.Integer)), false, nil
	case ilos.InstanceOf(instance.FloatClass, x):
		return float64(x.(instance.Float)), true, nil
	default:
		_, err := SignalCondition(e, instance.NewDomainError(e, x, instance.NumberClass), Nil)
		return 0.0, false, err
	}
}

func readFromString(s string) (ilos.Instance, ilos.Instance) {
	return parser.Parse(tokenizer.NewReader(strings.NewReader(s)))
}

func ensure(e ilos.Environment, c ilos.Class, i ...ilos.Instance) ilos.Instance {
	for _, o := range i {
		if !ilos.InstanceOf(c, o) {
			_, err := SignalCondition(e, instance.NewDomainError(e, o, c), Nil)
			return err
		}
	}
	return nil
}

var unique = 0

func uniqueInt() int {
	i := unique
	unique++
	return i
}

func func2symbol(function interface{}) ilos.Instance {
	name := runtime.FuncForPC(reflect.ValueOf(function).Pointer()).Name()
	name = regexp.MustCompile(`.*\.`).ReplaceAllString(name, "")
	name = regexp.MustCompile(`(.)([A-Z])`).ReplaceAllString(name, "$1-$2")
	name = strings.ToUpper(name)
	return instance.NewSymbol(name)
}
