// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"log"
	"runtime"
	"strings"

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
	return New(ArithmeticErrorClass, map[ilos.Instance]ilos.Instance{
		Symbol("OPERATION"): operation,
		Symbol("OPERANDS"):  operands,
	})
}

func NewDivisionByZero(operation, operands ilos.Instance) ilos.Instance {
	return New(DivisionByZeroClass, map[ilos.Instance]ilos.Instance{
		Symbol("OPERATION"): operation,
		Symbol("OPERANDS"):  operands,
	})
}

func NewParseError(str, expectedClass ilos.Instance) ilos.Instance {
	return New(ParseErrorClass, map[ilos.Instance]ilos.Instance{
		Symbol("STRING"):         str,
		Symbol("EXPECTED-CLASS"): expectedClass,
	})
}

func NewDomainError(object ilos.Instance, expectedClass ilos.Class) ilos.Instance {
	return New(DomainErrorClass, map[ilos.Instance]ilos.Instance{
		Symbol("CAUSE"):          Symbol("DOMAIN-ERROR"),
		Symbol("OBJECT"):         object,
		Symbol("EXPECTED-CLASS"): expectedClass,
	})
}

func NewUndefinedFunction(name ilos.Instance) ilos.Instance {
	return New(UndefinedFunctionClass, map[ilos.Instance]ilos.Instance{
		Symbol("NAME"):      name,
		Symbol("NAMESPACE"): Symbol("FUNCTION"),
	})
}

func NewUndefinedVariable(name ilos.Instance) ilos.Instance {
	return New(UndefinedVariableClass, map[ilos.Instance]ilos.Instance{
		Symbol("NAME"):      name,
		Symbol("NAMESPACE"): Symbol("VARIABLE"),
	})
}

func NewArityError() ilos.Instance {
	//stackTrace()
	return New(ProgramErrorClass, map[ilos.Instance]ilos.Instance{})
}

func NewIndexOutOfRange() ilos.Instance {
	//stackTrace()
	return New(ProgramErrorClass, map[ilos.Instance]ilos.Instance{})
}

func NewImmutableBinding() ilos.Instance {
	//stackTrace()
	return New(ProgramErrorClass, map[ilos.Instance]ilos.Instance{})
}

func NewSimpleError(formatString, formatArguments ilos.Instance) ilos.Instance {
	return New(SimpleErrorClass, map[ilos.Instance]ilos.Instance{
		Symbol("FORMAT-STRING"):    formatString,
		Symbol("FORMAT-ARGUMENTS"): formatArguments,
	})
}

func NewControlError() ilos.Instance {
	return New(ControlErrorClass)
}
