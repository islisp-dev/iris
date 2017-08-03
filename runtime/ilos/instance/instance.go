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

func (i *instance) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	if symbol, ok := key.(Symbol); ok {
		v, ok := i.slots[string(symbol)]
		return v, ok
	}
	return nil, false
}

func (i *instance) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	if symbol, ok := key.(Symbol); ok {
		if _, ok := i.slots[string(symbol)]; ok {
			i.slots[string(symbol)] = value
			return true
		}
	}
	return false
}

func (i *instance) String() string {
	class := fmt.Sprint(i.class)
	return fmt.Sprintf("#%v %v>", class[:len(class)-1], i.slots)
}
