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

func (i Character) GetSlotValue(key string) ilos.Instance {
	return nil
}

func (i Character) SetSlotValue(key string, value ilos.Instance) {
}

func (i Character) String() string {
	return string(i)
}
