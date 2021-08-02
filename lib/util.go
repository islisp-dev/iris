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
	"github.com/islisp-dev/iris/core"
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

func createList(es ...core.Instance) core.Instance {
	if len(es) == 0 {
		return Nil
	}
	return core.NewCons(es[0], createList(es[1:]...))
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
	return parser.Parse(e, tokenizer.NewBufferedTokenReader(strings.NewReader(s)))
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

type Pattern func(core.Instance) bool

func Sym(ss ...string) Pattern {
	return func(instance core.Instance) bool {
		if len(ss) == 0 {
			return core.InstanceOf(core.SymbolClass, instance)
		}
		for _, s := range ss {
			if core.DeepEqual(s, instance) {
				return true
			}
		}
		return false
	}
}

func Any(_ core.Instance) bool {
	return true
}

func Tpl(tests ...Pattern) Pattern {
	return func(instance core.Instance) bool {
		if !isProperList(instance) {
			return false
		}
		list := instance.(core.List).Slice()
		if len(list) != len(tests) {
			return false
		}
		for i := 0; i < len(list); i++ {
			if !tests[i](list[i]) {
				return false
			}
		}
		return true
	}
}

func Disj(tests ...Pattern) Pattern {
	return func(instance core.Instance) bool {
		for _, test := range tests {
			if test(instance) {
				return true
			}
		}
		return false
	}
}

func Conj(tests ...Pattern) Pattern {
	return func(instance core.Instance) bool {
		for _, test := range tests {
			if !test(instance) {
				return false
			}
		}
		return true
	}
}

func Neg(test Pattern) Pattern {
	return func(instance core.Instance) bool {
		return !test(instance)
	}
}

func Apd(test Pattern, tests ...Pattern) Pattern {
	return func(instance core.Instance) bool {
		if !isProperList(instance) {
			return false
		}
		list := instance.(core.List).Slice()
		for i, instance := range list {
			if test(instance) {
				if len(tests) > 1 && Apd(tests[0], tests...)(createList(list[i:]...)) {
					return true
				}
			}
		}
		return len(tests) == 0
	}
}

func Rep(test Pattern) Pattern {
	return func(instance core.Instance) bool {
		if !isProperList(instance) {
			return false
		}
		for _, instance := range instance.(core.List).Slice() {
			if !test(instance) {
				return false
			}
		}
		return true
	}
}
