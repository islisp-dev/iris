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

func (a *GeneralArrayStar) GetSlotValue(key string) ilos.Instance {
	if key == "LENGTH" {
		var r []ilos.Instance
		for d := range a.dimension {
			if d != 0 {
				r = append(r, NewInteger(d))
			}
		}
		v := NewGeneralVector(len(r))
		for i, d := range r {
			v.(GeneralVector)[i] = d
		}
		return v
	}
	return nil
}

func (a *GeneralArrayStar) SetSlotValue(key string, value ilos.Instance) {
}

func (a *GeneralArrayStar) String() string {
	return fmt.Sprint(a.array)
}

func UnsafeGeneralArrayStarAccess(a ilos.Instance, k ilos.Instance) ilos.Instance {
	var dim [128]int
	for i, d := range k.(GeneralVector) {
		dim[i] = int(d.(Integer))
	}
	return a.(*GeneralArrayStar).array[dim]
}

func UnsafeGeneralArrayStarAsign(a ilos.Instance, k ilos.Instance, v ilos.Instance) {
	var dim [128]int
	for i, d := range k.(GeneralVector) {
		dim[i] = int(d.(Integer))
	}
	a.(*GeneralArrayStar).array[dim] = v
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

func (i GeneralVector) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (i GeneralVector) SetSlotValue(key string, value ilos.Instance) {
}

func (i GeneralVector) String() string {
	return fmt.Sprint([]ilos.Instance(i))
}

func UnsafeGeneralVectorAccess(a ilos.Instance, k ilos.Instance) ilos.Instance {
	return a.(GeneralVector)[k.(Integer)]
}

func UnsafeGeneralVectorAsign(a ilos.Instance, k ilos.Instance, v ilos.Instance) {
	a.(GeneralVector)[k.(Integer)] = v
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

func (i String) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (i String) SetSlotValue(key string, value ilos.Instance) {
}

func (i String) String() string {
	return string(i)
}
