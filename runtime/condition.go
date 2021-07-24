// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

func SignalCondition(e ilos.Environment, condition, continuable ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, ilos.SeriousConditionClass, condition); err != nil {
		return nil, err
	}
	condition.(ilos.BasicInstance).SetSlotValue(ilos.NewSymbol("IRIS.CONTINUABLE"), continuable, ilos.SeriousConditionClass)
	_, c := e.Handler.(ilos.Applicable).Apply(e, condition)
	if ilos.InstanceOf(ilos.ContinueClass, c) {
		o, _ := c.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.OBJECT"), ilos.ContinueClass)
		return o, nil
	}
	return nil, c
}

func Cerror(e ilos.Environment, continueString, errorString ilos.Instance, objs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	arguments, err := List(e, objs...)
	if err != nil {
		return nil, err
	}
	condition := ilos.Create(e, ilos.SimpleErrorClass, ilos.NewSymbol("FORMAT-STRING"), errorString, ilos.NewSymbol("FORAMT-OBJECTS"), arguments)
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

func Error(e ilos.Environment, continueString, errorString ilos.Instance, objs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	arguments, err := List(e, objs...)
	if err != nil {
		return nil, err
	}
	condition := ilos.Create(e, ilos.SimpleErrorClass, ilos.NewSymbol("FORMAT-STRING"), errorString, ilos.NewSymbol("FORAMT-OBJECTS"), arguments)
	return SignalCondition(e, condition, Nil)
}

func IgnoreErrors(e ilos.Environment, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := Progn(e, forms...)
	if err != nil && ilos.InstanceOf(ilos.ErrorClass, err) {
		return Nil, nil
	}
	return ret, err
}

func ReportCondition(e ilos.Environment, condition, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	return Format(e, e.StandardOutput, ilos.NewString([]rune("~A")), condition)
}

func ConditionContinuable(e ilos.Environment, condition ilos.Instance) (ilos.Instance, ilos.Instance) {
	if continuable, ok := condition.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.CONTINUABLE"), ilos.SeriousConditionClass); ok {
		return continuable, nil
	}
	return Nil, nil
}

func ContinueCondition(e ilos.Environment, condition ilos.Instance, value ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if b, ok := condition.(ilos.BasicInstance).GetSlotValue(ilos.NewSymbol("IRIS.CONTINUABLE"), ilos.SeriousConditionClass); !ok || b == Nil {
		return nil, ilos.Create(e, ilos.ProgramErrorClass)
	}
	if len(value) == 1 {
		return nil, ilos.Create(e, ilos.ContinueClass, ilos.NewSymbol("IRIS.OBJECT"), value[0])
	}
	if len(value) == 0 {
		return nil, ilos.Create(e, ilos.ContinueClass, ilos.NewSymbol("IRIS.OBJECT"), Nil)
	}
	return nil, ilos.Create(e, ilos.ProgramErrorClass)
}

func WithHandler(e ilos.Environment, handler ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	fun, err := Eval(e, handler)
	if err != nil {
		return nil, err
	}
	e.Handler = fun
	ret, err := Progn(e, forms...)
	if err != nil {
		return nil, err
	}
	return ret, err
}
