package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
)

//
// instance
//

type instance struct {
	class ilos.Class
	slots map[string]ilos.Instance
}

func (i *instance) Class() ilos.Class {
	return i.class
}

func (i *instance) GetSlotValue(key ilos.Instance) ilos.Instance {
	v, _ := i.slots[string(key.(Symbol))]
	return v
}

func (i *instance) SetSlotValue(key ilos.Instance, value ilos.Instance) {
	i.slots[string(key.(Symbol))] = value
}

func (i *instance) String() string {
	return fmt.Sprintf("#INSTANCE[%v %v]", i.class, i.slots)
}
