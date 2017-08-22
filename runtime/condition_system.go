// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func SignalCondition(local, global environment.Environment, condition, continuable ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.SeriousCondition, condition); err != nil {
		return nil, err
	}
	condition.(instance.Instance).SetSlotValue(instance.NewSymbol("IRIS.CONTINUABLE"), continuable, class.SeriousCondition)
	return nil, condition
}

func Cerror(local, global environment.Environment, continueString, errorString ilos.Instance, objs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	arguments, err := List(local, global, objs...)
	if err != nil {
		return nil, err
	}
	condition := instance.Create(local, global, class.SimpleError, instance.NewSymbol("FORMAT-STRING"), errorString, instance.NewSymbol("FORAMT-OBJECTS"), arguments)
	ss, err := CreateStringOutputStream(local, global)
	if err != nil {
		return nil, err
	}
	if _, err := Format(local, global, ss, continueString, objs...); err != nil {
		return nil, err
	}
	continuable, err := GetOutputStreamString(local, global, ss)
	if err != nil {
		return nil, err
	}
	return SignalCondition(local, global, condition, continuable)
}

func Error(local, global environment.Environment, continueString, errorString ilos.Instance, objs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	arguments, err := List(local, global, objs...)
	if err != nil {
		return nil, err
	}
	condition := instance.Create(local, global, class.SimpleError, instance.NewSymbol("FORMAT-STRING"), errorString, instance.NewSymbol("FORAMT-OBJECTS"), arguments)
	return SignalCondition(local, global, condition, Nil)
}

func IgnoreError(local, global environment.Environment, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	ret, err := Progn(local, global, forms...)
	if err != nil && ilos.InstanceOf(class.Error, err) {
		return Nil, nil
	}
	return ret, err
}

func ReportCondition(local, global environment.Environment, condition, stream ilos.Instance) (ilos.Instance, ilos.Instance) {
	return nil, nil
}

func ConditionContinuable(local, global environment.Environment, condition ilos.Instance) (ilos.Instance, ilos.Instance) {
	if continuable, ok := condition.(instance.Instance).GetSlotValue(instance.NewSymbol("IRIS.CONTINUABLE"), class.SeriousCondition); ok {
		return continuable, nil
	}
	return Nil, nil
}

func ContinueCondition(local, global environment.Environment, condition, value ilos.Instance) (ilos.Instance, ilos.Instance) {
	// TODO:
	return nil, nil
}

func WithHandler(local, global environment.Environment, handler ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	fun, err := Eval(local, global, handler)
	if err != nil {
		return nil, err
	}
	ret, err := Progn(local, global, forms...)
	if err != nil {
		return fun.(instance.Applicable).Apply(local, global, err)
	}
	return ret, err
}
