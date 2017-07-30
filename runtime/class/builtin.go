package class

type builtin struct {
	parents []Class
	name    string
}

func (c *builtin) String() string {
	return c.name
}

func (c *builtin) Parents() []Class {
	return c.parents
}

func (c *builtin) Class() Class {
	return BuiltInClass
}

func (c *builtin) Value() Value {
	return c
}

func (c *builtin) New(value ...Value) Instance {
	return &defaultInstance{c, value[0]}
}

func (c *builtin) IsInstanceOf(class Class) bool {
	return isInstanceOf(c, class)
}

var Object = &builtin{[]Class{}, "<object>"}
var BasicArray = New(BuiltInClass, Object, "<basic-array>").(Class)
var BasicArrayStar = New(BuiltInClass, BasicArray, "<basic-array*>").(Class)
var GeneralArrayStar = New(BuiltInClass, BasicArrayStar, "<general-array*>").(Class)
var BasicVector = New(BuiltInClass, BasicArray, "<basic-vector>").(Class)
var GeneraVector = New(BuiltInClass, BasicVector, "<general-vector>").(Class)
var String = New(BuiltInClass, BasicVector, "<string>").(Class)
var Character = New(BuiltInClass, Object, "<character>").(Class)
var Function = New(BuiltInClass, Object, "<function>").(Class)
var GenericFunction = New(BuiltInClass, Function, "<generic-function>").(Class)
var StandardGenericFunction = New(BuiltInClass, GenericFunction, "<standard-generic-function>").(Class)
var List = New(BuiltInClass, Object, "<list>").(Class)
var Cons = New(BuiltInClass, List, "<cons>").(Class)
var Null = New(BuiltInClass, List, "<null>").(Class)
var Symbol = New(BuiltInClass, Object, "<symbol>").(Class)
var Number = New(BuiltInClass, Object, "<number>").(Class)
var Integer = New(BuiltInClass, Number, "<integer>").(Class)
var Float = New(BuiltInClass, Number, "<float>").(Class)
var SeriousCondition = New(BuiltInClass, Object, "<serious-condition>").(Class)
var Error = New(BuiltInClass, SeriousCondition, "<error>").(Class)
var ArithmeticError = New(BuiltInClass, Error, "<arithmetic-error>").(Class)
var DivisionByZero = New(BuiltInClass, ArithmeticError, "<division-by-zero>").(Class)
var FloatingPointOnderflow = New(BuiltInClass, ArithmeticError, "<floating-point-overflow>").(Class)
var FloatingPointUnderflow = New(BuiltInClass, ArithmeticError, "<floating-point-underflow>").(Class)
var ControlError = New(BuiltInClass, Error, "<control-error>").(Class)
var ParseError = New(BuiltInClass, Error, "<parse-error>").(Class)
var ProgramError = New(BuiltInClass, Error, "<program-error>").(Class)
var DomainError = New(BuiltInClass, ProgramError, "<domain-error>").(Class)
var UndefinedEntity = New(BuiltInClass, ProgramError, "<undefined-entity>").(Class)
var UndefinedVariable = New(BuiltInClass, UndefinedEntity, "<undefined-variable>").(Class)
var UndefinedFunction = New(BuiltInClass, UndefinedEntity, "<undefined-function>").(Class)
var SimpleError = New(BuiltInClass, Error, "<simple-error>").(Class)
var StreamError = New(BuiltInClass, Error, "<stream-error>").(Class)
var EndOfStream = New(BuiltInClass, StreamError, "<end-of-stream>").(Class)
var StorageExhausted = New(BuiltInClass, SeriousCondition, "<storage-exhausted>").(Class)
var StandardObject = New(BuiltInClass, Object, "<standard-object>").(Class)
var Stream = New(BuiltInClass, Object, "<stream>").(Class)
