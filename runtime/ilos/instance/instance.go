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

func (i *instance) GetSlotValue(key string) ilos.Instance {
	v, _ := i.slots[key]
	return v
}

func (i *instance) SetSlotValue(key string, value ilos.Instance) {
	i.slots[key] = value
}

func (i *instance) String() string {
	return fmt.Sprintf("#INSTANCE[%v %v]", i.class, i.slots)
}
