// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"log"
	"runtime"
	"strings"

	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
)

func stackTrace() {
	println("----")
	programCounter, sourceFileName, sourceFileLineNum, ok := runtime.Caller(2)
	log.Printf("programCounter: %v\n", programCounter)
	log.Printf("souruntime: %s\n", sourceFileName)
	log.Printf("sourceFileLineNum: %d\n", sourceFileLineNum)
	log.Printf("ok: %t\n", ok)

	fn := runtime.FuncForPC(programCounter)
	log.Printf("Function Name: %s\n", fn.Name())
	fileName, fileLine := fn.FileLine(programCounter)
	log.Printf("FileName:%s, FileLine: %d\n", fileName, fileLine)

	splitedFnName := strings.Split(fn.Name(), ".")
	packageName := splitedFnName[0]
	callerFuncName := splitedFnName[1]
	log.Printf("packageName: %s\n", packageName)
	log.Printf("functionName: %s\n", callerFuncName)
	println("-----")
}

func NewArithmeticError(operation, operands ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil),
		ArithmeticErrorClass,
		NewSymbol("OPERATION"), operation,
		NewSymbol("OPERANDS"), operands)
}

func NewDivisionByZero(operation, operands ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil),
		DivisionByZeroClass,
		NewSymbol("OPERATION"), operation,
		NewSymbol("OPERANDS"), operands)
}

func NewParseError(str, expectedClass ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil),
		ParseErrorClass,
		NewSymbol("STRING"), str,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewDomainError(object ilos.Instance, expectedClass ilos.Class) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil),
		DomainErrorClass,
		NewSymbol("CAUSE"), NewSymbol("DOMAIN-ERROR"),
		NewSymbol("OBJECT"), object,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewUndefinedFunction(name ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil),
		UndefinedFunctionClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("FUNCTION"))
}

func NewUndefinedVariable(name ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil),
		UndefinedVariableClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("VARIABLE"))
}

func NewUndefinedClass(name ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil),
		UndefinedEntityClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("CLASS"))
}

func NewArityError() ilos.Instance {
	stackTrace()
	return Create(env.NewEnvironment(nil, nil, nil), ProgramErrorClass)
}

func NewIndexOutOfRange() ilos.Instance {
	stackTrace()
	return Create(env.NewEnvironment(nil, nil, nil), ProgramErrorClass)
}

func NewImmutableBinding() ilos.Instance {
	stackTrace()
	return Create(env.NewEnvironment(nil, nil, nil), ProgramErrorClass)
}

func NewSimpleError(formatString, formatArguments ilos.Instance) ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil),
		SimpleErrorClass,
		NewSymbol("FORMAT-STRING"), formatString,
		NewSymbol("FORMAT-ARGUMENTS"), formatArguments)
}

func NewControlError() ilos.Instance {
	return Create(env.NewEnvironment(nil, nil, nil), ControlErrorClass)
}
