package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// Integer
//

type Integer int

func NewInteger(n int) ilos.Instance {
	return Integer(n)
}

func (Integer) Class() ilos.Class {
	return class.Integer
}

func (i Integer) GetSlotValue(key ilos.Instance) ilos.Instance {
	return nil
}

func (i Integer) SetSlotValue(key ilos.Instance, value ilos.Instance) {
	return
}

func (i Integer) String() string {
	return fmt.Sprint(int(i))
}

//
// Float
//

type Float float64

func NewFloat(n float64) ilos.Instance {
	return Float(n)
}

func (Float) Class() ilos.Class {
	return class.Float
}

func (i Float) GetSlotValue(key ilos.Instance) ilos.Instance {
	return nil
}

func (i Float) SetSlotValue(key ilos.Instance, value ilos.Instance) {
	return
}

func (i Float) String() string {
	return fmt.Sprint(float64(i))
}
