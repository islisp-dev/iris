// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"log"
	"runtime"
	"strings"

	"github.com/ta2gch/iris/runtime/environment"
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
	return Create(environment.New(), environment.New(),
		ArithmeticErrorClass,
		NewSymbol("OPERATION"), operation,
		NewSymbol("OPERANDS"), operands)
}

func NewDivisionByZero(operation, operands ilos.Instance) ilos.Instance {
	return Create(environment.New(), environment.New(),
		DivisionByZeroClass,
		NewSymbol("OPERATION"), operation,
		NewSymbol("OPERANDS"), operands)
}

func NewParseError(str, expectedClass ilos.Instance) ilos.Instance {
	return Create(environment.New(), environment.New(),
		ParseErrorClass,
		NewSymbol("STRING"), str,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewDomainError(object ilos.Instance, expectedClass ilos.Class) ilos.Instance {
	return Create(environment.New(), environment.New(),
		DomainErrorClass,
		NewSymbol("CAUSE"), NewSymbol("DOMAIN-ERROR"),
		NewSymbol("OBJECT"), object,
		NewSymbol("EXPECTED-CLASS"), expectedClass)
}

func NewUndefinedFunction(name ilos.Instance) ilos.Instance {
	return Create(environment.New(), environment.New(),
		UndefinedFunctionClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("FUNCTION"))
}

func NewUndefinedVariable(name ilos.Instance) ilos.Instance {
	return Create(environment.New(), environment.New(),
		UndefinedVariableClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("VARIABLE"))
}

func NewUndefinedClass(name ilos.Instance) ilos.Instance {
	return Create(environment.New(), environment.New(),
		UndefinedEntityClass,
		NewSymbol("NAME"), name,
		NewSymbol("NAMESPACE"), NewSymbol("CLASS"))
}

func NewArityError() ilos.Instance {
	return Create(environment.New(), environment.New(), ProgramErrorClass)
}

func NewIndexOutOfRange() ilos.Instance {
	return Create(environment.New(), environment.New(), ProgramErrorClass)
}

func NewImmutableBinding() ilos.Instance {
	return Create(environment.New(), environment.New(), ProgramErrorClass)
}

func NewSimpleError(formatString, formatArguments ilos.Instance) ilos.Instance {
	return Create(environment.New(), environment.New(),
		SimpleErrorClass,
		NewSymbol("FORMAT-STRING"), formatString,
		NewSymbol("FORMAT-ARGUMENTS"), formatArguments)
}

func NewControlError() ilos.Instance {
	return Create(environment.New(), environment.New(), ControlErrorClass)
}
