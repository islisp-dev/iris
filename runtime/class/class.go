package class

type Class struct {
	Parents []*Class
	name    string
}

var Object = &Class{[]*Class{}, "<object>"}
var BuiltInClass = &Class{[]*Class{Object}, "<built-in-class>"}
var StandardClass = &Class{[]*Class{Object}, "<standard-class>"}
var ArithmeticError = &Class{[]*Class{Object}, "<arithmetic-error>"}
var FloatingPointUnderflow = &Class{[]*Class{Object}, "<floating-point-underflow>"}
var SimpleError = &Class{[]*Class{Object}, "<simple-error>"}

var BasicArray = &Class{[]*Class{Object}, "<basic-array>"}
var BasicArrayStar = &Class{[]*Class{BasicArray}, "<basic-array*>"}
var GeneralArrayStar = &Class{[]*Class{BasicArrayStar}, "<general-array*>"}
var BasicVector = &Class{[]*Class{BasicArray}, "<basic-vector>"}
var GeneraVector = &Class{[]*Class{BasicVector}, "<general-vector>"}
var String = &Class{[]*Class{BasicVector}, "<string>"}

var Function = &Class{[]*Class{Object}, "<function>"}
var GenericFunction = &Class{[]*Class{Function}, "<generic-function>"}
var StandardGenericFunction = &Class{[]*Class{GenericFunction}, "<standard-generic-function>"}

var Character = &Class{[]*Class{Object}, "<character>"}

var Number = &Class{[]*Class{Object}, "<number>"}
var Integer = &Class{[]*Class{Number}, "<integer>"}
var Float = &Class{[]*Class{Number}, "<float>"}

var Symbol = &Class{[]*Class{Object}, "<symbol>"}

var List = &Class{[]*Class{Object}, "<list>"}
var Cons = &Class{[]*Class{List}, "<cons>"}
var Null = &Class{[]*Class{List}, "<null>"}
