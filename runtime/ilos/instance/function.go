package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

type Function func(ilos.Instance, environment.Environment, environment.Environment) (ilos.Instance, ilos.Instance)

func NewFunction(f func(ilos.Instance, environment.Environment, environment.Environment) (ilos.Instance, ilos.Instance)) ilos.Instance {
	return Function(f)
}

func (Function) Class() ilos.Class {
	return class.Function
}

func (Function) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (Function) SetSlotValue(key string, value ilos.Instance) {
}

func (f Function) String() string {
	return fmt.Sprintf("#instance[%v]", f.Class())
}

func UnsafeFunctionCall(f ilos.Instance, a ilos.Instance, l environment.Environment, g environment.Environment) (ilos.Instance, ilos.Instance) {
	success, fail := f.(Function)(a, l, g)
	return success, fail
}

type GenericFunction map[[128]ilos.Class]Function

func (GenericFunction) Class() ilos.Class {
	return class.GenericFunction
}

func (GenericFunction) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (GenericFunction) SetSlotValue(key string, value ilos.Instance) {
}

func (f GenericFunction) String() string {
	return fmt.Sprintf("#instance[%v]", f.Class())
}

func UnsafeGenericFunctionCall(f ilos.Instance, a ilos.Instance, l environment.Environment, g environment.Environment) (ilos.Instance, ilos.Instance) {
	var types [128]ilos.Class
	cdr := a
	idx := 0
	for ilos.InstanceOf(cdr, class.Cons) {
		types[idx] = cdr.Class()
		idx++
	}
	success, fail := f.(GenericFunction)[types](a, l, g)
	return success, fail
}

type StandardGenericFunction map[[128]ilos.Class]Function

func (StandardGenericFunction) Class() ilos.Class {
	return class.StandardGenericFunction
}

func (StandardGenericFunction) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (StandardGenericFunction) SetSlotValue(key string, value ilos.Instance) {
}

func (f StandardGenericFunction) String() string {
	return fmt.Sprintf("#instance[%v]", f.Class())
}

func UnsafeStandardGenericFunctionCall(f ilos.Instance, a ilos.Instance, l environment.Environment, g environment.Environment) (ilos.Instance, ilos.Instance) {
	var types [128]ilos.Class
	cdr := a
	idx := 0
	for ilos.InstanceOf(cdr, class.Cons) {
		types[idx] = cdr.Class()
		idx++
	}
	success, fail := f.(StandardGenericFunction)[types](a, l, g)
	return success, fail
}
