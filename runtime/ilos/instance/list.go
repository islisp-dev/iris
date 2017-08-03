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

func (i *Cons) GetSlotValue(key ilos.Instance) ilos.Instance {
	return nil
}

func (i *Cons) SetSlotValue(key ilos.Instance, value ilos.Instance) {
}

func (i *Cons) String() string {
	str := "(" + fmt.Sprint(i.car)
	cdr := i.cdr
	for ilos.InstanceOf(cdr, class.Cons) {
		str += fmt.Sprintf(" %v", UnsafeCar(cdr))
		cdr = UnsafeCdr(cdr)
	}
	if ilos.InstanceOf(cdr, class.Null) {
		str += ")"
	} else {
		str += fmt.Sprintf(" . %v)", cdr)
	}
	return str
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

func (i *Null) GetSlotValue(key ilos.Instance) ilos.Instance {
	return nil
}

func (i *Null) SetSlotValue(key ilos.Instance, value ilos.Instance) {
}

func (*Null) String() string {
	return "nil"
}
