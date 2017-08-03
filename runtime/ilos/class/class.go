package class

import "github.com/ta2gch/iris/runtime/ilos"

type builtinclass struct {
	parents []ilos.Class
	name    string
}

func (*builtinclass) Class() ilos.Class {
	return BuiltInClass
}

func (p *builtinclass) Parents() []ilos.Class {
	return p.parents
}

func (i *builtinclass) GetSlotValue(key ilos.Instance) ilos.Instance {
	return nil
}

func (i *builtinclass) SetSlotValue(key ilos.Instance, value ilos.Instance) {
}

func (p *builtinclass) String() string {
	return p.name
}

var Object = &builtinclass{[]ilos.Class{}, "<object>"}
var BuiltInClass = &builtinclass{[]ilos.Class{Object}, "<built-in-class>"}
var StandardClass = &builtinclass{[]ilos.Class{Object}, "<standard-class>"}
var BasicArray = &builtinclass{[]ilos.Class{Object}, "<basic-array>"}
var BasicArrayStar = &builtinclass{[]ilos.Class{BasicArray}, "<basic-array*>"}
var GeneralArrayStar = &builtinclass{[]ilos.Class{BasicArrayStar}, "<general-array*>"}
var BasicVector = &builtinclass{[]ilos.Class{BasicArray}, "<basic-vector>"}
var GeneraVector = &builtinclass{[]ilos.Class{BasicVector}, "<general-vector>"}
var String = &builtinclass{[]ilos.Class{BasicVector}, "<string>"}
var Character = &builtinclass{[]ilos.Class{Object}, "<character>"}
var Function = &builtinclass{[]ilos.Class{Object}, "<function>"}
var GenericFunction = &builtinclass{[]ilos.Class{Function}, "<generic-function>"}
var StandardGenericFunction = &builtinclass{[]ilos.Class{GenericFunction}, "<standard-generic-function>"}
var List = &builtinclass{[]ilos.Class{Object}, "<list>"}
var Cons = &builtinclass{[]ilos.Class{List}, "<cons>"}
var Null = &builtinclass{[]ilos.Class{List}, "<null>"}
var Symbol = &builtinclass{[]ilos.Class{Object}, "<symbol>"}
var Number = &builtinclass{[]ilos.Class{Object}, "<number>"}
var Integer = &builtinclass{[]ilos.Class{Number}, "<integer>"}
var Float = &builtinclass{[]ilos.Class{Number}, "<float>"}
var SeriousCondition = &builtinclass{[]ilos.Class{Object}, "<serious-condition>"}
var Error = &builtinclass{[]ilos.Class{SeriousCondition}, "<error>"}
var ArithmeticError = &builtinclass{[]ilos.Class{Error}, "<arithmetic-error>"}
var DivisionByZero = &builtinclass{[]ilos.Class{ArithmeticError}, "<division-by-zero>"}
var FloatingPointOnderflow = &builtinclass{[]ilos.Class{ArithmeticError}, "<floating-point-overflow>"}
var FloatingPointUnderflow = &builtinclass{[]ilos.Class{ArithmeticError}, "<floating-point-underflow>"}
var ControlError = &builtinclass{[]ilos.Class{Error}, "<control-error>"}
var ParseError = &builtinclass{[]ilos.Class{Error}, "<parse-error>"}
var ProgramError = &builtinclass{[]ilos.Class{Error}, "<program-error>"}
var DomainError = &builtinclass{[]ilos.Class{ProgramError}, "<domain-error>"}
var UndefinedEntity = &builtinclass{[]ilos.Class{ProgramError}, "<undefined-entity>"}
var UndefinedVariable = &builtinclass{[]ilos.Class{UndefinedEntity}, "<undefined-variable>"}
var UndefinedFunction = &builtinclass{[]ilos.Class{UndefinedEntity}, "<undefined-function>"}
var SimpleError = &builtinclass{[]ilos.Class{Error}, "<simple-error>"}
var StreamError = &builtinclass{[]ilos.Class{Error}, "<stream-error>"}
var EndOfStream = &builtinclass{[]ilos.Class{StreamError}, "<end-of-stream>"}
var StorageExhausted = &builtinclass{[]ilos.Class{SeriousCondition}, "<storage-exhausted>"}
var StandardObject = &builtinclass{[]ilos.Class{Object}, "<standard-object>"}
var Stream = &builtinclass{[]ilos.Class{Object}, "<stream>"}
