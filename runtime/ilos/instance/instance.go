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
	if _, ok := i.slots[key]; ok {
		i.slots[key] = value
		return true
	}
	return false
}

func (i *instance) String() string {
	class := fmt.Sprint(i.class)
	return fmt.Sprintf("#%v %v>", class[:len(class)-1], i.slots)
}
