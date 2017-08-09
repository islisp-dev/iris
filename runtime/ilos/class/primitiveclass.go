package class

import "github.com/ta2gch/iris/runtime/ilos"

type primitiveclass struct {
	parents []ilos.Class
	name    string
}

func (*primitiveclass) Class() ilos.Class {
	return BuiltInClass
}

func (p *primitiveclass) Parents() []ilos.Class {
	return p.parents
}

func (p *primitiveclass) Slots() []string {
	return []string{}
}

func (i *primitiveclass) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (i *primitiveclass) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (p *primitiveclass) String() string {
	return p.name
}

var Object = &primitiveclass{[]ilos.Class{}, "<OBJECT>"}
var BuiltInClass = &primitiveclass{[]ilos.Class{Object}, "<BUILT-IN-CLASS>"}
var StandardClass = &primitiveclass{[]ilos.Class{Object}, "<STANDARD-CLASS>"}
var BasicArray = &primitiveclass{[]ilos.Class{Object}, "<BASIC-ARRAY>"}
var BasicArrayStar = &primitiveclass{[]ilos.Class{BasicArray}, "<BASIC-ARRAY*>"}
var GeneralArrayStar = &primitiveclass{[]ilos.Class{BasicArrayStar}, "<GENERAL-ARRAY*>"}
var BasicVector = &primitiveclass{[]ilos.Class{BasicArray}, "<BASIC-VECTOR>"}
var GeneraVector = &primitiveclass{[]ilos.Class{BasicVector}, "<GENERAL-VECTOR>"}
var String = &primitiveclass{[]ilos.Class{BasicVector}, "<STRING>"}
var Character = &primitiveclass{[]ilos.Class{Object}, "<CHARACTER>"}
var Function = &primitiveclass{[]ilos.Class{Object}, "<FUNCTION>"}
var GenericFunction = &primitiveclass{[]ilos.Class{Function}, "<GENERIC-FUNCTION>"}
var StandardGenericFunction = &primitiveclass{[]ilos.Class{GenericFunction}, "<STANDARD-GENERIC-FUNCTION>"}
var List = &primitiveclass{[]ilos.Class{Object}, "<LIST>"}
var Cons = &primitiveclass{[]ilos.Class{List}, "<CONS>"}
var Null = &primitiveclass{[]ilos.Class{List}, "<NULL>"}
var Symbol = &primitiveclass{[]ilos.Class{Object}, "<SYMBOL>"}
var Number = &primitiveclass{[]ilos.Class{Object}, "<NUMBER>"}
var Integer = &primitiveclass{[]ilos.Class{Number}, "<INTEGER>"}
var Float = &primitiveclass{[]ilos.Class{Number}, "<FLOAT>"}
