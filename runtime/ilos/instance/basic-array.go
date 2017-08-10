package instance

import (
	"github.com/k0kubun/pp"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// General Vector
//

type GeneralVector []ilos.Instance

func (GeneralVector) Class() ilos.Class {
	return class.GeneralVector
}

func (i GeneralVector) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	if symbol, ok := key.(Symbol); ok && symbol == "LENGTH" {
		return New(class.Integer, len(i)), true
	}
	if index, ok := key.(Integer); ok && int(index) < len(i) {
		return i[int(index)], true
	}
	return nil, false
}

func (i GeneralVector) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	if index, ok := key.(Integer); ok && int(index) < len(i) {
		i[int(index)] = value
		return true
	}
	return false
}

func (i GeneralVector) String() string {
	return pp.Sprint([]ilos.Instance(i))
}

//
// String
//

type String []rune

func (String) Class() ilos.Class {
	return class.String
}

func (i String) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	if symbol, ok := key.(Symbol); ok && symbol == "LENGTH" {
		return New(class.Integer, len(i)), true
	}
	if index, ok := key.(Integer); ok && int(index) < len(i) {
		return New(class.Character, i[int(index)]), true
	}
	return nil, false
}

func (i String) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	if index, ok := key.(Integer); ok && int(index) < len(i) {
		if character, ok := value.(Character); ok {
			i[index] = rune(character)
			return true
		}
	}
	return false
}

func (i String) String() string {
	return "\"" + string(i) + "\""
}
