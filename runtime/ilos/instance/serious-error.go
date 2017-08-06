package instance

import (
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// ParseError
//

func NewArithmeticError(operation, operands ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.ArithmeticError
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("OPERATION")] = operation
	i.slots[NewSymbol("OPERANDS")] = operands
	return i
}

func NewDomainError(object, expectedClass ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.DomainError
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("STRING")] = object
	i.slots[NewSymbol("EXPECTED-CLASS")] = expectedClass
	return i
}

func NewParseError(str, expectedClass ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.ParseError
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("STRING")] = str
	i.slots[NewSymbol("EXPECTED-CLASS")] = expectedClass
	return i
}

func NewSimpleError(formatString, formatArguments ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.SimpleError
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("FORMAT-STRING")] = formatString
	i.slots[NewSymbol("FORMAT-ARGUMENTS")] = formatArguments
	return i
}

func NewStreamError(stream ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.StreamError
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("STREAM")] = stream
	return i
}

func NewUndefinedEntityError(name, namespace ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.UndefinedEntity
	i.slots = map[ilos.Instance]ilos.Instance{}
	i.slots[NewSymbol("NAME")] = name
	i.slots[NewSymbol("NAMESPACE")] = namespace
	return i
}
