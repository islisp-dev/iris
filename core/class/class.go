package class

type Class interface {
	ToString() string
	Parents() []Instance
	New(...Value) Instance
}

func New(c Instance, value ...Value) Instance {
	return c.(Class).New(value...)
}

type meta struct {
	name string
}

func (m *meta) ToString() string {
	return m.name
}

func (*meta) Parents() []Instance {
	return []Instance{Object}
}

func (*meta) Class() Instance {
	return BuiltInClass
}

func (m *meta) Value() Value {
	return m
}

func (m *meta) New(value ...Value) Instance {
	return &builtin{[]Instance{value[0].(Instance)}, value[1].(string)}
}

type builtin struct {
	parents []Instance
	name    string
}

func (c *builtin) ToString() string {
	return c.name
}

func (c *builtin) Parents() []Instance {
	return c.parents
}

func (c *builtin) Class() Instance {
	return BuiltInClass
}

func (c *builtin) Value() Value {
	return c
}

func (c *builtin) New(value ...Value) Instance {
	return &DefaultInstance{c, value[0]}
}

var Object = &builtin{[]Instance{}, "<object>"}
var BasicArray = New(BuiltInClass, Object, "<basic-array>")
var BasicArrayStar = New(BuiltInClass, BasicArray, "<basic-array*>")
var GeneralArrayStar = New(BuiltInClass, BasicArrayStar, "<general-array*>")
var BasicVector = New(BuiltInClass, BasicArray, "<basic-vector>")
var GeneraVector = New(BuiltInClass, BasicVector, "<general-vector>")
var String = New(BuiltInClass, BasicVector, "<string>")
var BuiltInClass = &meta{"<built-in-class>"}
var Character = New(BuiltInClass, Object, "<character>")
var Function = New(BuiltInClass, Object, "<function>")
var GenericFunction = New(BuiltInClass, Function, "<generic-function>")
var StandardGenericFunction = New(BuiltInClass, GenericFunction, "<standard-generic-function>")
var List = New(BuiltInClass, Object, "<list>")
var Cons = New(BuiltInClass, List, "<cons>")
var Null = New(BuiltInClass, List, "<null>")
var Symbol = New(BuiltInClass, Object, "<symbol>")
var Number = New(BuiltInClass, Object, "<number>")
var Integer = New(BuiltInClass, Number, "<integer>")
var Float = New(BuiltInClass, Number, "<float>")
var SeriousCondition = New(BuiltInClass, Object, "<serious-condition>")
var Error = New(BuiltInClass, SeriousCondition, "<error>")
var ArithmeticError = New(BuiltInClass, Error, "<arithmetic-error>")
var DivisionByZero = New(BuiltInClass, ArithmeticError, "<division-by-zero>")
var FloatingPointOnderflow = New(BuiltInClass, ArithmeticError, "<floating-point-overflow>")
var FloatingPointUnderflow = New(BuiltInClass, ArithmeticError, "<floating-point-underflow>")
var ControlError = New(BuiltInClass, Error, "<control-error>")
var ParseError = New(BuiltInClass, Error, "<parse-error>")
var ProgramError = New(BuiltInClass, Error, "<program-error>")
var DomainError = New(BuiltInClass, ProgramError, "<domain-error>")
var UndefinedEntity = New(BuiltInClass, ProgramError, "<undefined-entity>")
var UndefinedVariable = New(BuiltInClass, UndefinedEntity, "<undefined-variable>")
var UndefinedFunction = New(BuiltInClass, UndefinedEntity, "<undefined-function>")
var SimpleError = New(BuiltInClass, Error, "<simple-error>")
var StreamError = New(BuiltInClass, Error, "<stream-error>")
var EndOfStream = New(BuiltInClass, StreamError, "<end-of-stream>")
var StorageExhausted = New(BuiltInClass, SeriousCondition, "<storage-exhausted>")
var StandardClass = &meta{"<standard-class>"}
var StandardObject = New(BuiltInClass, Object, "<standard-object>")
var Stream = New(BuiltInClass, Object, "<stream>")
