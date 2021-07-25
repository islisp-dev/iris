// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package core

var DefaultHandler = NewFunction(NewSymbol("DEFAULT-HANDLER"), func(e Environment, c Instance) (Instance, Instance) {
	return nil, c
})

func SignalCondition(e Environment, condition, continuable Instance) (Instance, Instance) {
	if !InstanceOf(SeriousConditionClass, condition) {
		return SignalCondition(e, NewDomainError(e, condition, SeriousConditionClass), Nil)
	}
	condition.(BasicInstance).SetSlotValue(NewSymbol("IRIS.CONTINUABLE"), continuable, SeriousConditionClass)
	_, c := e.Handler.(Applicable).Apply(e, condition)
	if InstanceOf(ContinueClass, c) {
		o, _ := c.(BasicInstance).GetSlotValue(NewSymbol("IRIS.OBJECT"), ContinueClass)
		return o, nil
	}
	return nil, c
}

func NewEndOfStream(e Environment) Instance {
	return Create(e, EndOfStreamClass)
}

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
		NewSymbol("OBJECT"), object,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewUndefinedFunction(e Environment, name Instance) Instance {
	l, c := -1, -1
	if s, ok := name.(Symbol); ok {
		l, c = s.Location()
	}
	loc := NewCons(NewInteger(l), NewInteger(c))
	return Create(e, UndefinedFunctionClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("FUNCTION"),
		NewSymbol("IRIS.STACKTRACE"), NewCons(loc, Nil))
}

func NewUnboundVariable(e Environment, name Instance) Instance {
	l, c := -1, -1
	if s, ok := name.(Symbol); ok {
		l, c = s.Location()
	}
	loc := NewCons(NewInteger(l), NewInteger(c))
	return Create(e, UnboundVariableClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("VARIABLE"),
		NewSymbol("IRIS.STACKTRACE"), NewCons(loc, Nil))
}

func NewUndefinedClass(e Environment, name Instance) Instance {
	l, c := -1, -1
	if s, ok := name.(Symbol); ok {
		l, c = s.Location()
	}
	loc := NewCons(NewInteger(l), NewInteger(c))
	return Create(e, UndefinedEntityClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("CLASS"),
		NewSymbol("IRIS.STACKTRACE"), NewCons(loc, Nil))
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

func NewStreamError(e Environment, stream Instance) Instance {
	return Create(e, StreamErrorClass, NewSymbol("STREAM"), stream)
}
