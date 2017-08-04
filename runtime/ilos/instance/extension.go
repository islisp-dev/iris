package instance

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

func NewThrow(tag, object ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.Throw
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("TAG")] = tag
	i.slots[NewSymbol("OBJECT")] = object
	return i
}

func NewTagBodyTag(tag ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.TagBodyTag
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("TAG")] = tag
	return i
}

func NewWrongNumberOfArguments(function, arguments ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.WrongNumberOfArguments
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("FORM")] = function
	i.slots[NewSymbol("ARGUMENTS")] = arguments
	return i
}

func NewUndefinedFunction(name ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.UndefinedFunction
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("NAME")] = name
	i.slots[NewSymbol("NAMESPACE")] = NewSymbol("FUNCTION")
	return i
}

func NewUndefinedVariable(name ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.UndefinedVariable
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("NAME")] = name
	i.slots[NewSymbol("NAMESPACE")] = NewSymbol("VARIABLE")
	return i
}
