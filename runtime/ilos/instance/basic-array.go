package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// General Array Star
//

type GeneralArrayStar struct {
	dimension [128]int
	array     map[[128]int]ilos.Instance
}

func NewGeneralArrayStar(key []ilos.Instance) ilos.Instance {
	dim := [128]int{}
	for i, d := range key {
		dim[i] = int(d.(Integer))
	}
	return &GeneralArrayStar{dim, map[[128]int]ilos.Instance{}}
}

func (*GeneralArrayStar) Class() ilos.Class {
	return class.GeneralArrayStar
}

func (a *GeneralArrayStar) GetSlotValue(key ilos.Instance) ilos.Instance {
	if ilos.InstanceOf(key, class.Symbol) && key.(Symbol) == "LENGTH" {
		cons := NewNull()
		for i := 128; i > 0; i-- {
			if a.dimension[i-1] != 0 {
				cons = NewCons(NewInteger(a.dimension[i-1]), cons)
			}
		}
		return cons
	}
	if ilos.InstanceOf(key, class.List) {
		dim := [128]int{}
		idx := 0
		cdr := key
		for ilos.InstanceOf(cdr, class.Cons) {
			dim[idx] = int(UnsafeCar(cdr).(Integer))
			cdr = UnsafeCdr(cdr)
			idx++
		}
		return a.array[dim]
	}
	return nil
}

func (a *GeneralArrayStar) SetSlotValue(key ilos.Instance, value ilos.Instance) {
	if ilos.InstanceOf(key, class.List) {
		dim := [128]int{}
		idx := 0
		cdr := key
		for ilos.InstanceOf(cdr, class.Cons) {
			dim[idx] = int(UnsafeCar(cdr).(Integer))
			cdr = UnsafeCdr(cdr)
			idx++
		}
		a.array[dim] = value
	}
}

func (a *GeneralArrayStar) String() string {
	return fmt.Sprint(a.array)
}

//
// General Vector
//

type GeneralVector []ilos.Instance

func NewGeneralVector(n int) ilos.Instance {
	return GeneralVector(make([]ilos.Instance, n))
}

func (GeneralVector) Class() ilos.Class {
	return class.GeneraVector
}

func (i GeneralVector) GetSlotValue(key ilos.Instance) ilos.Instance {
	if ilos.InstanceOf(key, class.Symbol) && key.(Symbol) == "LENGTH" {
		return NewInteger(len(i))
	}
	if ilos.InstanceOf(key, class.Integer) {
		return i[int(key.(Integer))]
	}
	return nil
}

func (i GeneralVector) SetSlotValue(key ilos.Instance, value ilos.Instance) {
	if ilos.InstanceOf(key, class.Integer) {
		i[int(key.(Integer))] = value
	}
}

func (i GeneralVector) String() string {
	return fmt.Sprint([]ilos.Instance(i))
}

//
// String
//

type String []rune

func NewString(a string) ilos.Instance {
	return String([]rune(a))
}

func (String) Class() ilos.Class {
	return class.String
}

func (i String) GetSlotValue(key ilos.Instance) ilos.Instance {
	if ilos.InstanceOf(key, class.Symbol) && key.(Symbol) == "LENGTH" {
		return NewInteger(len(i))
	}
	if ilos.InstanceOf(key, class.Integer) {
		return NewCharacter(i[int(key.(Integer))])
	}
	return nil
}

func (i String) SetSlotValue(key ilos.Instance, value ilos.Instance) {
	if ilos.InstanceOf(key, class.Integer) {
		i[int(key.(Integer))] = rune(value.(Character))
	}
}

func (i String) String() string {
	return string(i)
}
