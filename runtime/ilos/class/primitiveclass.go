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
