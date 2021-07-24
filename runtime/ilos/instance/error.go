// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

func NewArithmeticError(e ilos.Environment, operation, operands ilos.Instance) ilos.Instance {
	return Create(e, ArithmeticErrorClass,
		NewSymbol("OPERATION"), operation,
		NewSymbol("OPERANDS"), operands)
}

func NewDivisionByZero(e ilos.Environment, operation, operands ilos.Instance) ilos.Instance {
	return Create(e, DivisionByZeroClass,
		NewSymbol("OPERATION"), operation,
		NewSymbol("OPERANDS"), operands)
}

func NewParseError(e ilos.Environment, str, expectedClass ilos.Instance) ilos.Instance {
	return Create(e, ParseErrorClass,
		NewSymbol("STRING"), str,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewDomainError(e ilos.Environment, object ilos.Instance, expectedClass ilos.Class) ilos.Instance {
	return Create(e, DomainErrorClass,
		NewSymbol("CAUSE"), NewSymbol("DOMAIN-ERROR"),
		NewSymbol("IRIS.OBJECT"), object,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewUndefinedFunction(e ilos.Environment, name ilos.Instance) ilos.Instance {
	return Create(e, UndefinedFunctionClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("FUNCTION"))
}

func NewUndefinedVariable(e ilos.Environment, name ilos.Instance) ilos.Instance {
	return Create(e, UndefinedVariableClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("VARIABLE"))
}

func NewUndefinedClass(e ilos.Environment, name ilos.Instance) ilos.Instance {
	return Create(e, UndefinedEntityClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("CLASS"))
}

func NewArityError(e ilos.Environment) ilos.Instance {
	return Create(e, ProgramErrorClass)
}

func NewIndexOutOfRange(e ilos.Environment) ilos.Instance {
	return Create(e, ProgramErrorClass)
}

func NewImmutableBinding(e ilos.Environment) ilos.Instance {
	return Create(e, ProgramErrorClass)
}

func NewSimpleError(e ilos.Environment, formatString, formatArguments ilos.Instance) ilos.Instance {
	return Create(e, SimpleErrorClass,
		NewSymbol("FORMAT-STRING"), formatString,
		NewSymbol("FORMAT-ARGUMENTS"), formatArguments)
}

func NewControlError(e ilos.Environment) ilos.Instance {
	return Create(e, ControlErrorClass)
}

func NewStreamError(e ilos.Environment) ilos.Instance {
	return Create(e, StreamErrorClass)
}
