package class

import "github.com/ta2gch/iris/runtime/ilos"

type builtinclass struct {
	parents []ilos.Class
	slots   []string
	name    string
}

func (*builtinclass) Class() ilos.Class {
	return BuiltInClass
}

func (p *builtinclass) Parents() []ilos.Class {
	return p.parents
}

func (p *builtinclass) Slots() []string {
	return p.slots
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

var SeriousCondition = &builtinclass{[]ilos.Class{Object}, []string{}, "<SERIOUS-CONDITION>"}
var Error = &builtinclass{[]ilos.Class{SeriousCondition}, []string{}, "<ERROR>"}
var ArithmeticError = &builtinclass{[]ilos.Class{Error}, []string{"OPERATION", "OPERANDS"}, "<ARITHMETIC-ERROR>"}
var DivisionByZero = &builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<DIVISION-BY-ZERO>"}
var FloatingPointOnderflow = &builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<FLOATING-POINT-OVERFLOW>"}
var FloatingPointUnderflow = &builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<FLOATING-POINT-UNDERFLOW>"}
var ControlError = &builtinclass{[]ilos.Class{Error}, []string{}, "<CONTROL-ERROR>"}
var ParseError = &builtinclass{[]ilos.Class{Error}, []string{"STRING", "EXPECTED-CLASS"}, "<PARSE-ERROR>"}
var ProgramError = &builtinclass{[]ilos.Class{Error}, []string{}, "<PROGRAM-ERROR>"}
var DomainError = &builtinclass{[]ilos.Class{ProgramError}, []string{"OBJECT", "EXPECTED-CLASS"}, "<DOMAIN-ERROR>"}
var UndefinedEntity = &builtinclass{[]ilos.Class{ProgramError}, []string{"NAME", "NAMESPACE"}, "<UNDEFINED-ENTITY>"}
var UndefinedVariable = &builtinclass{[]ilos.Class{UndefinedEntity}, []string{}, "<UNDEFINED-VARIABLE>"}
var UndefinedFunction = &builtinclass{[]ilos.Class{UndefinedEntity}, []string{}, "<UNDEFINED-FUNCTION>"}
var SimpleError = &builtinclass{[]ilos.Class{Error}, []string{"FORMAT-STRING", "FORMAT-ARGUMENTS"}, "<SIMPLE-ERROR>"}
var StreamError = &builtinclass{[]ilos.Class{Error}, []string{}, "<STREAM-ERROR>"}
var EndOfStream = &builtinclass{[]ilos.Class{StreamError}, []string{}, "<END-OF-STREAM>"}
var StorageExhausted = &builtinclass{[]ilos.Class{SeriousCondition}, []string{}, "<STORAGE-EXHAUSTED>"}
var StandardObject = &builtinclass{[]ilos.Class{Object}, []string{}, "<STANDARD-OBJECT>"}
var Stream = &builtinclass{[]ilos.Class{Object}, []string{"STREAM"}, "<STREAM>"}

// Implementation defined
var WrongNumberOfArguments = &builtinclass{[]ilos.Class{Error}, []string{"FORM", "ARGUMENTS"}, "<WRONG-NUMBER-OF-ARGUMENTS>"}
var Escape = &builtinclass{[]ilos.Class{Object}, []string{"TAG"}, "<ESCAPE>"}
var CatchTag = &builtinclass{[]ilos.Class{Escape}, []string{"OBJECT"}, "<THROW>"}
var TagbodyTag = &builtinclass{[]ilos.Class{Escape}, []string{}, "<TAGBODY-TAG>"}
var BlockTag = &builtinclass{[]ilos.Class{Escape}, []string{"OBJECT"}, "<BLOCK-TAG>"}
