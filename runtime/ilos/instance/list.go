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

func (i *Cons) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (i *Cons) SetSlotValue(key string, value ilos.Instance) {
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

func (i *Null) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (i *Null) SetSlotValue(key string, value ilos.Instance) {
}

func (*Null) String() string {
	return "nil"
}
