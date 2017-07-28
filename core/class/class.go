package class

type Class struct {
	Parents []*Class
	name    string
}

func (class *Class) ToString() string {
	return class.name
}

func (class *Class) New(value interface{}) *Instance {
	return &Instance{class, value}
}

func NewClass(parent *Class, name string) *Class {
	return &Class{[]*Class{parent}, name}
}

var Object = &Class{[]*Class{}, "<object>"}
var BasicArray = NewClass(Object, "<basic-array>")
var BasicArrayStar = NewClass(BasicArray, "<basic-array*>")
var GeneralArrayStar = NewClass(BasicArrayStar, "<general-array*>")
var BasicVector = NewClass(BasicArray, "<basic-vector>")
var GeneraVector = NewClass(BasicVector, "<general-vector>")
var String = NewClass(BasicVector, "<string>")
var BuiltInClass = NewClass(Object, "<built-in-class>")
var Character = NewClass(Object, "<character>")
var Function = NewClass(Object, "<function>")
var GenericFunction = NewClass(Function, "<generic-function>")
var StandardGenericFunction = NewClass(GenericFunction, "<standard-generic-function>")
var List = NewClass(Object, "<list>")
var Cons = NewClass(List, "<cons>")
var Null = NewClass(List, "<null>")
var Symbol = NewClass(Object, "<symbol>")
var Number = NewClass(Object, "<number>")
var Integer = NewClass(Number, "<integer>")
var Float = NewClass(Number, "<float>")
var SeriousCondition = NewClass(Object, "<serious-condition>")
var Error = NewClass(SeriousCondition, "<error>")
var ArithmeticError = NewClass(Error, "<arithmetic-error>")
var DivisionByZero = NewClass(ArithmeticError, "<division-by-zero>")
var FloatingPointOnderflow = NewClass(ArithmeticError, "<floating-point-overflow>")
var FloatingPointUnderflow = NewClass(ArithmeticError, "<floating-point-underflow>")
var ControlError = NewClass(Error, "<control-error>")
var ParseError = NewClass(Error, "<parse-error>")
var ProgramError = NewClass(Error, "<program-error>")
var DomainError = NewClass(ProgramError, "<domain-error>")
var UndefinedEntity = NewClass(ProgramError, "<undefined-entity>")
var UndefinedVariable = NewClass(UndefinedEntity, "<undefined-variable>")
var UndefinedFunction = NewClass(UndefinedEntity, "<undefined-function>")
var SimpleError = NewClass(Error, "<simple-error>")
var StreamError = NewClass(Error, "<stream-error>")
var EndOfStream = NewClass(StreamError, "<end-of-stream>")
var StorageExhausted = NewClass(SeriousCondition, "<storage-exhausted>")
var StandardClass = NewClass(Object, "<standard-class>")
var StandardObject = NewClass(Object, "<standard-object>")
var Stream = NewClass(Object, "<stream>")
