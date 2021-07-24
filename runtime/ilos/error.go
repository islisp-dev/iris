// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package ilos

func NewArithmeticError(e Environment, operation, operands Instance) Instance {
	return Create(e, ArithmeticErrorClass,
		NewSymbol("OPERATION"), operation,
		NewSymbol("OPERANDS"), operands)
}

func NewDivisionByZero(e Environment, operation, operands Instance) Instance {
	return Create(e, DivisionByZeroClass,
		NewSymbol("OPERATION"), operation,
		NewSymbol("OPERANDS"), operands)
}

func NewParseError(e Environment, str, expectedClass Instance) Instance {
	return Create(e, ParseErrorClass,
		NewSymbol("STRING"), str,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewDomainError(e Environment, object Instance, expectedClass Class) Instance {
	return Create(e, DomainErrorClass,
		NewSymbol("CAUSE"), NewSymbol("DOMAIN-ERROR"),
		NewSymbol("IRIS.OBJECT"), object,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewUndefinedFunction(e Environment, name Instance) Instance {
	return Create(e, UndefinedFunctionClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("FUNCTION"))
}

func NewUndefinedVariable(e Environment, name Instance) Instance {
	return Create(e, UndefinedVariableClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("VARIABLE"))
}

func NewUndefinedClass(e Environment, name Instance) Instance {
	return Create(e, UndefinedEntityClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("CLASS"))
}

func NewArityError(e Environment) Instance {
	return Create(e, ProgramErrorClass)
}

func NewIndexOutOfRange(e Environment) Instance {
	return Create(e, ProgramErrorClass)
}

func NewImmutableBinding(e Environment) Instance {
	return Create(e, ProgramErrorClass)
}

func NewSimpleError(e Environment, formatString, formatArguments Instance) Instance {
	return Create(e, SimpleErrorClass,
		NewSymbol("FORMAT-STRING"), formatString,
		NewSymbol("FORMAT-ARGUMENTS"), formatArguments)
}

func NewControlError(e Environment) Instance {
	return Create(e, ControlErrorClass)
}

func NewStreamError(e Environment) Instance {
	return Create(e, StreamErrorClass)
}
