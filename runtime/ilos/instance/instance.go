package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// instance
//

type instance struct {
	class ilos.Class
	slots map[ilos.Instance]ilos.Instance
}

func (i *instance) Class() ilos.Class {
	return i.class
}

func (i *instance) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	v, ok := i.slots[key]
	return v, ok
}

func (i *instance) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	i.slots[key.(Symbol)] = value
	return true
}

func (i *instance) String() string {
	return fmt.Sprintf("#INSTANCE[%v %v]", i.class, i.slots)
}

//
// ParseError
//

func NewParseError(s string, ec ilos.Class) ilos.Instance {
	i := new(instance)
	i.class = class.ParseError
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("STRING")] = NewString(s)
	i.slots[NewSymbol("EXPECTED-CLASS")] = ec
	return i
}
