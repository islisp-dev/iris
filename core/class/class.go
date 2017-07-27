package class

type Class struct {
	Parents []*Class
	name    string
}

func (c *Class) ToString() string {
	return c.name
}

func (c *Class) New(val interface{}, rest ...interface{}) *Instance {
	return &Instance{c, val}
}

func defclass(parent *Class, name string) *Class {
	return &Class{[]*Class{parent}, name}
}

var Object = &Class{[]*Class{}, "<object>"}
var BuiltInClass = defclass(Object, "<built-in-class>")
var StandardClass = defclass(Object, "<standard-class>")
var ArithmeticError = defclass(Object, "<arithmetic-error>")
var FloatingPointUnderflow = defclass(Object, "<floating-point-underflow>")
var SimpleError = defclass(Object, "<simple-error>")
var BasicArray = defclass(Object, "<basic-array>")
var BasicArrayStar = defclass(BasicArray, "<basic-array*>")
var GeneralArrayStar = defclass(BasicArrayStar, "<general-array*>")
var BasicVector = defclass(BasicArray, "<basic-vector>")
var GeneraVector = defclass(BasicVector, "<general-vector>")
var String = defclass(BasicVector, "<string>")
var Function = defclass(Object, "<function>")
var GenericFunction = defclass(Function, "<generic-function>")
var StandardGenericFunction = defclass(GenericFunction, "<standard-generic-function>")
var Character = defclass(Object, "<character>")
var Number = defclass(Object, "<number>")
var Integer = defclass(Number, "<integer>")
var Float = defclass(Number, "<float>")
var Symbol = defclass(Object, "<symbol>")
var List = defclass(Object, "<list>")
var Cons = defclass(List, "<cons>")
var Null = defclass(List, "<null>")
