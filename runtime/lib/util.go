// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import (
	"reflect"
	"regexp"
	"runtime"
	"strings"

	"github.com/islisp-dev/iris/reader/parser"
	"github.com/islisp-dev/iris/reader/tokenizer"
	"github.com/islisp-dev/iris/runtime/core"
)

func isProperList(i core.Instance) bool {
	if core.InstanceOf(core.ConsClass, i) {
		return isProperList(i.(*core.Cons).Cdr) // Checked at the top of this statements
	}
	if core.DeepEqual(i, Nil) {
		return true
	}
	return false
}

func convFloat64(e core.Environment, x core.Instance) (float64, bool, core.Instance) {
	switch {
	case core.InstanceOf(core.IntegerClass, x):
		return float64(x.(core.Integer)), false, nil
	case core.InstanceOf(core.FloatClass, x):
		return float64(x.(core.Float)), true, nil
	default:
		_, err := SignalCondition(e, core.NewDomainError(e, x, core.NumberClass), Nil)
		return 0.0, false, err
	}
}

func readFromString(s string) (core.Instance, core.Instance) {
	e := core.NewEnvironment(nil, nil, nil, core.DefaultHandler)
	return parser.Parse(e, tokenizer.NewReader(strings.NewReader(s)))
}

func ensure(e core.Environment, c core.Class, i ...core.Instance) core.Instance {
	for _, o := range i {
		if !core.InstanceOf(c, o) {
			_, err := SignalCondition(e, core.NewDomainError(e, o, c), Nil)
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

func func2symbol(function interface{}) core.Instance {
	name := runtime.FuncForPC(reflect.ValueOf(function).Pointer()).Name()
	name = regexp.MustCompile(`.*\.`).ReplaceAllString(name, "")
	name = regexp.MustCompile(`(.)([A-Z])`).ReplaceAllString(name, "$1-$2")
	name = strings.ToUpper(name)
	return core.NewSymbol(name)
}
