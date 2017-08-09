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

func (i *builtinclass) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i *builtinclass) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (p *builtinclass) String() string {
	return p.name
}

var Object = &builtinclass{[]ilos.Class{}, "<OBJECT>"}
var BuiltInClass = &builtinclass{[]ilos.Class{Object}, "<BUILT-IN-CLASS>"}
var StandardClass = &builtinclass{[]ilos.Class{Object}, "<STANDARD-CLASS>"}
var BasicArray = &builtinclass{[]ilos.Class{Object}, "<BASIC-ARRAY>"}
var BasicArrayStar = &builtinclass{[]ilos.Class{BasicArray}, "<BASIC-ARRAY*>"}
var GeneralArrayStar = &builtinclass{[]ilos.Class{BasicArrayStar}, "<GENERAL-ARRAY*>"}
var BasicVector = &builtinclass{[]ilos.Class{BasicArray}, "<BASIC-VECTOR>"}
var GeneraVector = &builtinclass{[]ilos.Class{BasicVector}, "<GENERAL-VECTOR>"}
var String = &builtinclass{[]ilos.Class{BasicVector}, "<STRING>"}
var Character = &builtinclass{[]ilos.Class{Object}, "<CHARACTER>"}
var Function = &builtinclass{[]ilos.Class{Object}, "<FUNCTION>"}
var GenericFunction = &builtinclass{[]ilos.Class{Function}, "<GENERIC-FUNCTION>"}
var StandardGenericFunction = &builtinclass{[]ilos.Class{GenericFunction}, "<STANDARD-GENERIC-FUNCTION>"}
var List = &builtinclass{[]ilos.Class{Object}, "<LIST>"}
var Cons = &builtinclass{[]ilos.Class{List}, "<CONS>"}
var Null = &builtinclass{[]ilos.Class{List}, "<NULL>"}
var Symbol = &builtinclass{[]ilos.Class{Object}, "<SYMBOL>"}
var Number = &builtinclass{[]ilos.Class{Object}, "<NUMBER>"}
var Integer = &builtinclass{[]ilos.Class{Number}, "<INTEGER>"}
var Float = &builtinclass{[]ilos.Class{Number}, "<FLOAT>"}
var SeriousCondition = &builtinclass{[]ilos.Class{Object}, "<SERIOUS-CONDITION>"}
var Error = &builtinclass{[]ilos.Class{SeriousCondition}, "<ERROR>"}
var ArithmeticError = &builtinclass{[]ilos.Class{Error}, "<ARITHMETIC-ERROR>"}
var DivisionByZero = &builtinclass{[]ilos.Class{ArithmeticError}, "<DIVISION-BY-ZERO>"}
var FloatingPointOnderflow = &builtinclass{[]ilos.Class{ArithmeticError}, "<FLOATING-POINT-OVERFLOW>"}
var FloatingPointUnderflow = &builtinclass{[]ilos.Class{ArithmeticError}, "<FLOATING-POINT-UNDERFLOW>"}
var ControlError = &builtinclass{[]ilos.Class{Error}, "<CONTROL-ERROR>"}
var ParseError = &builtinclass{[]ilos.Class{Error}, "<PARSE-ERROR>"}
var ProgramError = &builtinclass{[]ilos.Class{Error}, "<PROGRAM-ERROR>"}
var DomainError = &builtinclass{[]ilos.Class{ProgramError}, "<DOMAIN-ERROR>"}
var UndefinedEntity = &builtinclass{[]ilos.Class{ProgramError}, "<UNDEFINED-ENTITY>"}
var UndefinedVariable = &builtinclass{[]ilos.Class{UndefinedEntity}, "<UNDEFINED-VARIABLE>"}
var UndefinedFunction = &builtinclass{[]ilos.Class{UndefinedEntity}, "<UNDEFINED-FUNCTION>"}
var SimpleError = &builtinclass{[]ilos.Class{Error}, "<SIMPLE-ERROR>"}
var StreamError = &builtinclass{[]ilos.Class{Error}, "<STREAM-ERROR>"}
var EndOfStream = &builtinclass{[]ilos.Class{StreamError}, "<END-OF-STREAM>"}
var StorageExhausted = &builtinclass{[]ilos.Class{SeriousCondition}, "<STORAGE-EXHAUSTED>"}
var StandardObject = &builtinclass{[]ilos.Class{Object}, "<STANDARD-OBJECT>"}
var Stream = &builtinclass{[]ilos.Class{Object}, "<STREAM>"}

// Implementation defined
var WrongNumberOfArguments = &builtinclass{[]ilos.Class{Error}, "<WRONG-NUMBER-OF-ARGUMENTS>"}
var Escape = &builtinclass{[]ilos.Class{Object}, "<ESCAPE>"}
var Throw = &builtinclass{[]ilos.Class{Escape}, "<THROW>"}
var Go = &builtinclass{[]ilos.Class{Escape}, "<TAGBODY-TAG>"}
var ReturnFrom = &builtinclass{[]ilos.Class{Escape}, "<BLOCK-TAG>"}
