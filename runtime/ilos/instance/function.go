package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

type Function func(ilos.Instance, *environment.Environment, *environment.Environment) (ilos.Instance, ilos.Instance)

func NewFunction(f func(ilos.Instance, *environment.Environment, *environment.Environment) (ilos.Instance, ilos.Instance)) ilos.Instance {
	return Function(f)
}

func (Function) Class() ilos.Class {
	return class.Function
}

func (f Function) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	return nil, false
}

func (Function) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	return false
}

func (f Function) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

type GenericFunction map[[128]ilos.Class]Function

func (GenericFunction) Class() ilos.Class {
	return class.GenericFunction
}

func (f GenericFunction) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	types := [128]ilos.Class{}
	cdr := key
	idx := 0
	for ilos.InstanceOf(cdr, class.Cons) {
		types[idx] = cdr.Class()
		idx++
	}
	if v, ok := f[types]; ok {
		return v, true
	}
	return nil, false
}

func (f GenericFunction) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	types := [128]ilos.Class{}
	cdr := key
	idx := 0
	for ilos.InstanceOf(cdr, class.Cons) {
		types[idx] = cdr.Class()
		idx++
	}
	f[types] = value.(Function)
	return true
}

func (f GenericFunction) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

type StandardGenericFunction map[[128]ilos.Class]Function

func (StandardGenericFunction) Class() ilos.Class {
	return class.StandardGenericFunction
}

func (f StandardGenericFunction) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	types := [128]ilos.Class{}
	cdr := key
	idx := 0
	for ilos.InstanceOf(cdr, class.Cons) {
		types[idx] = cdr.Class()
		idx++
	}
	if v, ok := f[types]; ok {
		return v, true
	}
	return nil, false
}

func (f StandardGenericFunction) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	types := [128]ilos.Class{}
	cdr := key
	idx := 0
	for ilos.InstanceOf(cdr, class.Cons) {
		types[idx] = cdr.Class()
		idx++
	}
	f[types] = value.(Function)
	return true
}

func (f StandardGenericFunction) String() string {
	return fmt.Sprintf("#%v", f.Class())
}
