// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/core"

func SignalCondition(e core.Environment, condition, continuable core.Instance) (core.Instance, core.Instance) {
	return core.SignalCondition(e, condition, continuable)
}

func Cerror(e core.Environment, continueString, errorString core.Instance, objs ...core.Instance) (core.Instance, core.Instance) {
	arguments, err := List(e, objs...)
	if err != nil {
		return nil, err
	}
	condition := core.NewSimpleError(e, errorString, arguments)
	ss, err := CreateStringOutputStream(e)
	if err != nil {
		return nil, err
	}
	if _, err := Format(e, ss, continueString, objs...); err != nil {
		return nil, err
	}
	continuable, err := GetOutputStreamString(e, ss)
	if err != nil {
		return nil, err
	}
	return SignalCondition(e, condition, continuable)
}

func Error(e core.Environment, errorString core.Instance, objs ...core.Instance) (core.Instance, core.Instance) {
	arguments, err := List(e, objs...)
	if err != nil {
		return nil, err
	}
	condition := core.NewSimpleError(e, errorString, arguments)
	return SignalCondition(e, condition, Nil)
}

func IgnoreErrors(e core.Environment, forms ...core.Instance) (core.Instance, core.Instance) {
	ret, err := Progn(e, forms...)
	if err != nil && core.InstanceOf(core.ErrorClass, err) {
		return Nil, nil
	}
	return ret, err
}

func ReportCondition(e core.Environment, condition, stream core.Instance) (core.Instance, core.Instance) {
	return Format(e, e.StandardOutput, core.NewString([]rune("~A")), condition)
}

func ConditionContinuable(e core.Environment, condition core.Instance) (core.Instance, core.Instance) {
	if continuable, ok := condition.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.CONTINUABLE"), core.SeriousConditionClass); ok {
		return continuable, nil
	}
	return Nil, nil
}

func ContinueCondition(e core.Environment, condition core.Instance, value ...core.Instance) (core.Instance, core.Instance) {
	if b, ok := condition.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.CONTINUABLE"), core.SeriousConditionClass); !ok || b == Nil {
		return nil, core.Create(e, core.ProgramErrorClass)
	}
	if len(value) == 1 {
		return nil, core.Create(e, core.ContinueClass, core.NewSymbol("IRIS.OBJECT"), value[0])
	}
	if len(value) == 0 {
		return nil, core.Create(e, core.ContinueClass, core.NewSymbol("IRIS.OBJECT"), Nil)
	}
	return nil, core.Create(e, core.ProgramErrorClass)
}

func WithHandler(e core.Environment, handler core.Instance, forms ...core.Instance) (core.Instance, core.Instance) {
	fun, err := Eval(e, handler)
	if err != nil {
		return nil, err
	}
	f := e.NewHandler(fun)
	ret, err := Progn(f, forms...)
	if err != nil {
		return nil, err
	}
	return ret, err
}

func CreateReader(class core.Class, key string) func(e core.Environment, c core.Instance) (core.Instance, core.Instance) {
	return func(e core.Environment, c core.Instance) (core.Instance, core.Instance) {
		if core.InstanceOf(class, c) {
			if v, ok := c.(core.BasicInstance).GetSlotValue(core.NewSymbol(key), core.ArithmeticErrorClass); ok {
				return v, nil
			}
			return Nil, nil
		}
		return SignalCondition(e, core.NewDomainError(e, c, class), Nil)
	}
}
