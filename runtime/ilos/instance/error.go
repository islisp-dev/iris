// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"log"
	"runtime"
	"strings"

	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
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
	return New(class.ArithmeticError, map[string]ilos.Instance{
		"OPERATION": operation,
		"OPERANDS":  operands,
	})
}

func NewDivisionByZero(operation, operands ilos.Instance) ilos.Instance {
	return New(class.DivisionByZero, map[string]ilos.Instance{
		"OPERATION": operation,
		"OPERANDS":  operands,
	})
}

func NewParseError(str, expectedClass ilos.Instance) ilos.Instance {
	return New(class.ParseError, map[string]ilos.Instance{
		"STRING":          str,
		"EXPECTED-CLASSS": expectedClass,
	})
}

func NewDomainError(object ilos.Instance, expectedClass ilos.Class) ilos.Instance {
	return New(class.DomainError, map[string]ilos.Instance{"CAUSE": Symbol("DOMAIN-ERROR"),
		"OBJECT":          object,
		"EXPECTED-CLASSS": expectedClass,
	})
}

func NewUndefinedFunction(name ilos.Instance) ilos.Instance {
	return New(class.UndefinedFunction, map[string]ilos.Instance{
		"NAME":      name,
		"NAMESPACE": Symbol("FUNCTION"),
	})
}

func NewUndefinedVariable(name ilos.Instance) ilos.Instance {
	return New(class.UndefinedVariable, map[string]ilos.Instance{
		"NAME":      name,
		"NAMESPACE": Symbol("VARIABLE"),
	})
}

func NewArityError() ilos.Instance {
	//stackTrace()
	return New(class.ProgramError, map[string]ilos.Instance{})
}

func NewIndexOutOfRange() ilos.Instance {
	//stackTrace()
	return New(class.ProgramError, map[string]ilos.Instance{})
}

func NewImmutableBinding() ilos.Instance {
	//stackTrace()
	return New(class.ProgramError, map[string]ilos.Instance{})
}

func NewSimpleError(formatString, formatArguments ilos.Instance) ilos.Instance {
	return New(class.SimpleError, map[string]ilos.Instance{
		"FORMAT-STRING":    formatString,
		"FORMAT-ARGUMENTS": formatArguments,
	})
}

func NewControlError() ilos.Instance {
	return New(class.ControlError)
}