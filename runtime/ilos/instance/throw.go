package instance

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

func NewThrow(operation, operands ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.Throw
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("TAG")] = operation
	i.slots[NewSymbol("OBJECT")] = operands
	return i
}
