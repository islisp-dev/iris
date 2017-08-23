// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func SignalCondition(e env.Environment, condition, continuable ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.SeriousCondition, condition); err != nil {
		return nil, err
	}
	if handler, ok := e.Handler.Get(instance.NewSymbol("HANDLER")); ok {
		if continuable != Nil {
			handler.(instance.Applicable).Apply(e, condition)
		}
	}
	condition.(instance.Instance).SetSlotValue(instance.NewSymbol("IRIS.CONTINUABLE"), continuable, class.SeriousCondition)
	return nil, condition
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
	return nil, nil
}

func ConditionContinuable(e env.Environment, condition ilos.Instance) (ilos.Instance, ilos.Instance) {
	if continuable, ok := condition.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.CONTINUABLE"), class.SeriousCondition); ok {
		return continuable, nil
	}
	return Nil, nil
}

func ContinueCondition(e env.Environment, condition, value ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO:
	return nil, nil
}

func WithHandler(e env.Environment, handler ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	fun, err := Eval(e, handler)
	if err != nil {
		return nil, err
	}
	ret, err := Progn(e, forms...)
	if err != nil {
		return fun.(instance.Applicable).Apply(e, err)
	}
	return ret, err
}
