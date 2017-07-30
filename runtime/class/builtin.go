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
	return IsInstanceOf(c, class)
}

var Object = &builtin{[]Class{}, "<object>"}
var BasicArray = BuiltInClass.New(Object, "<basic-array>").(Class)
var BasicArrayStar = BuiltInClass.New(BasicArray, "<basic-array*>").(Class)
var GeneralArrayStar = BuiltInClass.New(BasicArrayStar, "<general-array*>").(Class)
var BasicVector = BuiltInClass.New(BasicArray, "<basic-vector>").(Class)
var GeneraVector = BuiltInClass.New(BasicVector, "<general-vector>").(Class)
var String = BuiltInClass.New(BasicVector, "<string>").(Class)
var Character = BuiltInClass.New(Object, "<character>").(Class)
var Function = BuiltInClass.New(Object, "<function>").(Class)
var GenericFunction = BuiltInClass.New(Function, "<generic-function>").(Class)
var StandardGenericFunction = BuiltInClass.New(GenericFunction, "<standard-generic-function>").(Class)
var List = BuiltInClass.New(Object, "<list>").(Class)
var Cons = BuiltInClass.New(List, "<cons>").(Class)
var Null = BuiltInClass.New(List, "<null>").(Class)
var Symbol = BuiltInClass.New(Object, "<symbol>").(Class)
var Number = BuiltInClass.New(Object, "<number>").(Class)
var Integer = BuiltInClass.New(Number, "<integer>").(Class)
var Float = BuiltInClass.New(Number, "<float>").(Class)
var SeriousCondition = BuiltInClass.New(Object, "<serious-condition>").(Class)
var Error = BuiltInClass.New(SeriousCondition, "<error>").(Class)
var ArithmeticError = BuiltInClass.New(Error, "<arithmetic-error>").(Class)
var DivisionByZero = BuiltInClass.New(ArithmeticError, "<division-by-zero>").(Class)
var FloatingPointOnderflow = BuiltInClass.New(ArithmeticError, "<floating-point-overflow>").(Class)
var FloatingPointUnderflow = BuiltInClass.New(ArithmeticError, "<floating-point-underflow>").(Class)
var ControlError = BuiltInClass.New(Error, "<control-error>").(Class)
var ParseError = BuiltInClass.New(Error, "<parse-error>").(Class)
var ProgramError = BuiltInClass.New(Error, "<program-error>").(Class)
var DomainError = BuiltInClass.New(ProgramError, "<domain-error>").(Class)
var UndefinedEntity = BuiltInClass.New(ProgramError, "<undefined-entity>").(Class)
var UndefinedVariable = BuiltInClass.New(UndefinedEntity, "<undefined-variable>").(Class)
var UndefinedFunction = BuiltInClass.New(UndefinedEntity, "<undefined-function>").(Class)
var SimpleError = BuiltInClass.New(Error, "<simple-error>").(Class)
var StreamError = BuiltInClass.New(Error, "<stream-error>").(Class)
var EndOfStream = BuiltInClass.New(StreamError, "<end-of-stream>").(Class)
var StorageExhausted = BuiltInClass.New(SeriousCondition, "<storage-exhausted>").(Class)
var StandardObject = BuiltInClass.New(Object, "<standard-object>").(Class)
var Stream = BuiltInClass.New(Object, "<stream>").(Class)
