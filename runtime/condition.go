// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/xtaniguchimasaya/iris/runtime/env"
	"github.com/xtaniguchimasaya/iris/runtime/ilos"
	"github.com/xtaniguchimasaya/iris/runtime/ilos/class"
	"github.com/xtaniguchimasaya/iris/runtime/ilos/instance"
)

func SignalCondition(e env.Environment, condition, continuable ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, class.SeriousCondition, condition); err != nil {
		return nil, err
	}
	condition.(instance.Instance).SetSlotValue(instance.NewSymbol("IRIS.CONTINUABLE"), continuable, class.SeriousCondition)
	_, c := e.Handler.(instance.Applicable).Apply(e, condition)
	if ilos.InstanceOf(class.Continue, c) {
		o, _ := c.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.OBJECT"), class.Continue)
		return o, nil
	}
	return nil, c
}

func Cerror(e env.Environment, continueString, errorString ilos.Instance, objs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	arguments, err := List(e, objs...)
	if err != nil {
		return nil, err
	}
	condition := instance.Create(e, class.SimpleError, instance.NewSymbol("FORMAT-STRING"), errorString, instance.NewSymbol("FORAMT-OBJECTS"), arguments)
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

func Error(e env.Environment, continueString, errorString ilos.Instance, objs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	arguments, err := List(e, objs...)
	if err != nil {
		return nil, err
	}
	condition := instance.Create(e, class.SimpleError, instance.NewSymbol("FORMAT-STRING"), errorString, instance.NewSymbol("FORAMT-OBJECTS"), arguments)
	return SignalCondition(e, condition, Nil)
}

func IgnoreError(e env.Environment, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := Progn(e, forms...)
	if err != nil && ilos.InstanceOf(class.Error, err) {
		return Nil, nil
	}
	return ret, err
}

func ReportCondition(e env.Environment, condition, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	return Format(e, e.StandardOutput, instance.NewString([]rune("~A")), condition)
}

func ConditionContinuable(e env.Environment, condition ilos.Instance) (ilos.Instance, ilos.Instance) {
	if continuable, ok := condition.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.CONTINUABLE"), class.SeriousCondition); ok {
		return continuable, nil
	}
	return Nil, nil
}

func ContinueCondition(e env.Environment, condition ilos.Instance, value ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if b, ok := condition.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.CONTINUABLE"), class.SeriousCondition); !ok || b == Nil {
		return nil, instance.Create(e, class.ProgramError)
	}
	if len(value) == 1 {
		return nil, instance.Create(e, class.Continue, instance.NewSymbol("IRIS.OBJECT"), value[0])
	}
	if len(value) == 0 {
		return nil, instance.Create(e, class.Continue, instance.NewSymbol("IRIS.OBJECT"), Nil)
	}
	return nil, instance.Create(e, class.ProgramError)
}

func WithHandler(e env.Environment, handler ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
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
