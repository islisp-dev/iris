package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// Cons
//

type Cons struct {
	car ilos.Instance
	cdr ilos.Instance
}

func NewCons(car ilos.Instance, cdr ilos.Instance) ilos.Instance {
	return &Cons{car, cdr}
}

func (*Cons) Class() ilos.Class {
	return class.Cons
}

func (i *Cons) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	if !ilos.InstanceOf(key, class.Symbol) {
		return nil, false
	}
	switch key.(Symbol) {
	case "CAR":
		return i.car, true
	case "CDR":
		return i.cdr, true
	default:
		return nil, false
	}
}

func (i *Cons) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	if !ilos.InstanceOf(key, class.Symbol) {
		return false
	}
	switch key.(Symbol) {
	case "CAR":
		i.car = value
		return true
	case "CDR":
		i.cdr = value
		return true
	default:
		return false
	}
}

func (i *Cons) String() string {
	return fmt.Sprintf("(%v . %v)", i.car, i.cdr)
}

func UnsafeCar(i ilos.Instance) ilos.Instance {
	return i.(*Cons).car
}

func UnsafeCdr(i ilos.Instance) ilos.Instance {
	return i.(*Cons).cdr
}

//
// Null
//

type Null struct{}

func NewNull() ilos.Instance {
	return &Null{}
}

func (*Null) Class() ilos.Class {
	return class.Null
}

func (i *Null) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	return nil, false
}

func (i *Null) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	return false
}

func (*Null) String() string {
	return "nil"
}
