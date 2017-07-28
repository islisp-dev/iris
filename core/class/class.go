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
var BuiltInClass = NewClass(Object, "<built-in-class>")
var StandardClass = NewClass(Object, "<standard-class>")
var ArithmeticError = NewClass(Object, "<arithmetic-error>")
var FloatingPointUnderflow = NewClass(Object, "<floating-point-underflow>")
var SimpleError = NewClass(Object, "<simple-error>")
var BasicArray = NewClass(Object, "<basic-array>")
var BasicArrayStar = NewClass(BasicArray, "<basic-array*>")
var GeneralArrayStar = NewClass(BasicArrayStar, "<general-array*>")
var BasicVector = NewClass(BasicArray, "<basic-vector>")
var GeneraVector = NewClass(BasicVector, "<general-vector>")
var String = NewClass(BasicVector, "<string>")
var Function = NewClass(Object, "<function>")
var GenericFunction = NewClass(Function, "<generic-function>")
var StandardGenericFunction = NewClass(GenericFunction, "<standard-generic-function>")
var Character = NewClass(Object, "<character>")
var Number = NewClass(Object, "<number>")
var Integer = NewClass(Number, "<integer>")
var Float = NewClass(Number, "<float>")
var Symbol = NewClass(Object, "<symbol>")
var List = NewClass(Object, "<list>")
var Cons = NewClass(List, "<cons>")
var Null = NewClass(List, "<null>")
