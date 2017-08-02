package instance

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// Character
//

type Character rune

func NewCharacter(n rune) ilos.Instance {
	return Character(n)
}

func (Character) Class() ilos.Class {
	return class.Character
}

func (i Character) GetSlotValue(key ilos.Instance) (ilos.Instance, bool) {
	return nil, false
}

func (i Character) SetSlotValue(key ilos.Instance, value ilos.Instance) bool {
	return false
}
func (i Character) String() string {
	return string(i)
}
