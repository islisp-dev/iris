package instance

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// Symbol
//

type Symbol string

func NewSymbol(n string) ilos.Instance {
	return Symbol(n)
}

func (Symbol) Class() ilos.Class {
	return class.Symbol
}

func (i Symbol) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (i Symbol) SetSlotValue(key string, value ilos.Instance) {
	return
}

func (i Symbol) String() string {
	return string(i)
}
