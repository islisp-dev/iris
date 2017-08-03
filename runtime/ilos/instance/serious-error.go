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
	i.slots = map[string]ilos.Instance{}
	i.slots["OPERATION"] = operation
	i.slots["OPERANDS"] = operands
	return i
}

func NewDomainError(object, expectedClass ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.DomainError
	i.slots = map[string]ilos.Instance{}
	i.slots["STRING"] = object
	i.slots["EXPECTED-CLASS"] = expectedClass
	return i
}

func NewParseError(str, expectedClass ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.ParseError
	i.slots = map[string]ilos.Instance{}
	i.slots["STRING"] = str
	i.slots["EXPECTED-CLASS"] = expectedClass
	return i
}

func NewSimpleError(formatString, formatArguments ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.SimpleError
	i.slots = map[string]ilos.Instance{}
	i.slots["FORMAT-STRING"] = formatString
	i.slots["FORMAT-ARGUMENTS"] = formatArguments
	return i
}

func NewStreamError(stream ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.StreamError
	i.slots = map[string]ilos.Instance{}
	i.slots["stream"] = stream
	return i
}

func NewUndefinedEntityError(name, namespace ilos.Instance) ilos.Instance {
	i := new(instance)
	i.class = class.UndefinedEntity
	i.slots = map[string]ilos.Instance{}
	i.slots["NAME"] = name
	i.slots["NAMESPACE"] = namespace
	return i
}
