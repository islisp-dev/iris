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

func (f Function) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (Function) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (f Function) String() string {
	return fmt.Sprintf("#%v", f.Class())
}
