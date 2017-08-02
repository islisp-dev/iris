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

func NewGeneralArrayStar(key ilos.Instance) ilos.Instance {
	dim := [128]int{}
	cdr := key
	idx := 0
	if !ilos.InstanceOf(key, class.List) {
		return nil
	}
	if cdr.Class() == class.Null {
		return &GeneralArrayStar{dim, map[[128]int]ilos.Instance{}}
	}
	for ilos.Instance(cdr, class.Cons) {
		car := UnsafeCar(cdr)
		cdr = UnsafeCdr(cdr)
		if idx >= 128 || !ilos.InstanceOf(car, class.Integer) || !ilos.InstanceOf(cdr, class.List) || int(car.(Integer)) > 0 {
			return nil
		}
		dim[idx] = int(car.(Integer))
		idx++
	}
	return &GeneralArrayStar{dim, map[[128]int]ilos.Instance{}}
}

func (*GeneralArrayStar) Class() ilos.Class {
	return class.GeneralArrayStar
}

func (a *GeneralArrayStar) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	dim := [128]int{}
	cdr := key
	idx := 0
	if !ilos.InstanceOf(key, class.List) {
		return nil, false
	}
	if a.dimension == dim && key.Class() == class.Null {
		return a.array[dim], true
	}
	for ilos.Instance(cdr, class.Cons) {
		car := UnsafeCar(cdr)
		cdr = UnsafeCdr(cdr)
		if idx >= 128 || !ilos.InstanceOf(car, class.Integer) || !ilos.InstanceOf(cdr, class.List) {
			return nil, false
		}
		dim[idx] = int(car.(Integer))
		idx++
	}
	for i := range a.dimension {
		if 0 >= dim[i] || dim[i] >= a.dimension[i] {
			return nil, false
		}
	}
	return a.array[dim], true
}

func (a *GeneralArrayStar) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	dim := [128]int{}
	cdr := key
	idx := 0
	if !ilos.InstanceOf(key, class.List) {
		return false
	}
	if a.dimension == dim && ilos.InstanceOf(key, class.Null) {
		a.array[dim] = value
		return true
	}
	for ilos.InstanceOf(cdr, class.Cons) {
		car := UnsafeCar(cdr)
		cdr = UnsafeCdr(cdr)
		if idx >= 128 || !ilos.InstanceOf(key, class.Integer) || !ilos.InstanceOf(cdr, class.List) {
			return false
		}
		dim[idx] = int(car.(Integer))
		idx++
	}
	for i := range a.dimension {
		if 0 >= dim[i] || dim[i] >= a.dimension[i] {
			return false
		}
	}
	a.array[dim] = value
	return true
}

func (a *GeneralArrayStar) String() string {
	return fmt.Sprint(a.array)
}

//
// General Vector
//

type GeneralVector []ilos.Instance

func NewGeneralVector(i []ilos.Instance) ilos.Instance {
	return GeneralVector(i)
}

func (GeneralVector) Class() ilos.Class {
	return class.GeneraVector
}

func (i GeneralVector) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	if ilos.InstanceOf(key, class.Symbol) && key.(Symbol) == "length" {
		return Integer(len(i)), true
	}
	if ilos.InstanceOf(key, class.Integer) && int(key.(Integer)) < len(i) {
		return i[key.(Integer)], true
	}
	return nil, false
}

func (i GeneralVector) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	if ilos.InstanceOf(key, class.Integer) && int(key.(Integer)) < len(i) {
		i[key.(Integer)] = value
		return true
	}
	return false
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

func (i String) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	if ilos.InstanceOf(key, class.Symbol) && key.(Symbol) == "length" {
		return Integer(len(i)), true
	}
	if ilos.InstanceOf(key, class.Integer) && int(key.(Integer)) < len(i) {
		return Character(i[key.(Integer)]), true
	}
	return nil, false
}

func (i String) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	if ilos.InstanceOf(key, class.Integer) && int(key.(Integer)) < len(i) {
		i[key.(Integer)] = rune(value.(Character))
		return true
	}
	return false
}

func (i String) String() string {
	return string(i)
}
