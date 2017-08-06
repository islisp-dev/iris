package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
)

//
// instance
//

type instance struct {
	class  ilos.Class
	supers []ilos.Instance
	slots  map[ilos.Instance]ilos.Instance
}

func (i *instance) Class() ilos.Class {
	return i.class
}

func (i *instance) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	if v, ok := i.slots[key]; ok {
		return v, ok
	}
	for _, s := range i.supers {
		if v, ok := s.GetSlotValue(key); ok {
			return v, ok
		}
	}
	return nil, false
}

func (i *instance) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	if _, ok := i.slots[key]; ok {
		i.slots[key] = value
		return true
	}
	for _, s := range i.supers {
		if ok := s.SetSlotValue(key, value); ok {
			return ok
		}
	}
	return false
}

func (i *instance) String() string {
	class := fmt.Sprint(i.class)
	return fmt.Sprintf("#%v %v>", class[:len(class)-1], i.slots)
}
