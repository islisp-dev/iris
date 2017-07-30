package class

type Class interface {
	ToString() string
	Parents() []Class
	New(...Value) Instance
}

func New(c Instance, value ...Value) Instance {
	return c.(Class).New(value...)
}

type MetaClass struct {
	name string
}

func (m *MetaClass) ToString() string {
	return m.name
}

func (*MetaClass) Parents() []Class {
	return []Class{Object.Value().(Class)}
}

func (*MetaClass) Class() Instance {
	return Object
}

func (m *MetaClass) Value() Value {
	return m
}

func (m *MetaClass) New(value ...Value) Instance {
	return &builtInClass{[]Class{value[0].(Instance).Value().(Class)}, value[1].(string)}
}

type builtInClass struct {
	parents []Class
	name    string
}

func (c *builtInClass) ToString() string {
	return c.name
}

func (c *builtInClass) Parents() []Class {
	return c.parents
}

func (c *builtInClass) Class() Instance {
	return BuiltInClass
}

func (c *builtInClass) Value() Value {
	return c
}

func (c *builtInClass) New(value ...Value) Instance {
	return &DefaultInstance{c, value[0]}
}

var Object = &builtInClass{[]Class{}, "<object>"}
var BasicArray = BuiltInClass.New(Object, "<basic-array>")
var BasicArrayStar = BuiltInClass.New(BasicArray, "<basic-array*>")
var GeneralArrayStar = BuiltInClass.New(BasicArrayStar, "<general-array*>")
var BasicVector = BuiltInClass.New(BasicArray, "<basic-vector>")
var GeneraVector = BuiltInClass.New(BasicVector, "<general-vector>")
var String = BuiltInClass.New(BasicVector, "<string>")
var BuiltInClass = &MetaClass{"<built-in-class>"}
var Character = BuiltInClass.New(Object, "<character>")
var Function = BuiltInClass.New(Object, "<function>")
var GenericFunction = BuiltInClass.New(Function, "<generic-function>")
var StandardGenericFunction = BuiltInClass.New(GenericFunction, "<standard-generic-function>")
var List = BuiltInClass.New(Object, "<list>")
var Cons = BuiltInClass.New(List, "<cons>")
var Null = BuiltInClass.New(List, "<null>")
var Symbol = BuiltInClass.New(Object, "<symbol>")
var Number = BuiltInClass.New(Object, "<number>")
var Integer = BuiltInClass.New(Number, "<integer>")
var Float = BuiltInClass.New(Number, "<float>")
var SeriousCondition = BuiltInClass.New(Object, "<serious-condition>")
var Error = BuiltInClass.New(SeriousCondition, "<error>")
var ArithmeticError = BuiltInClass.New(Error, "<arithmetic-error>")
var DivisionByZero = BuiltInClass.New(ArithmeticError, "<division-by-zero>")
var FloatingPointOnderflow = BuiltInClass.New(ArithmeticError, "<floating-point-overflow>")
var FloatingPointUnderflow = BuiltInClass.New(ArithmeticError, "<floating-point-underflow>")
var ControlError = BuiltInClass.New(Error, "<control-error>")
var ParseError = BuiltInClass.New(Error, "<parse-error>")
var ProgramError = BuiltInClass.New(Error, "<program-error>")
var DomainError = BuiltInClass.New(ProgramError, "<domain-error>")
var UndefinedEntity = BuiltInClass.New(ProgramError, "<undefined-entity>")
var UndefinedVariable = BuiltInClass.New(UndefinedEntity, "<undefined-variable>")
var UndefinedFunction = BuiltInClass.New(UndefinedEntity, "<undefined-function>")
var SimpleError = BuiltInClass.New(Error, "<simple-error>")
var StreamError = BuiltInClass.New(Error, "<stream-error>")
var EndOfStream = BuiltInClass.New(StreamError, "<end-of-stream>")
var StorageExhausted = BuiltInClass.New(SeriousCondition, "<storage-exhausted>")
var StandardClass = &MetaClass{"<standard-class>"}
var StandardObject = BuiltInClass.New(Object, "<standard-object>")
var Stream = BuiltInClass.New(Object, "<stream>")
