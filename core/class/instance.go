package class

type Value interface{}

// Instance struct type is the struct for the internal representations
type Instance interface {
	Class() Class
	Value() Value
	IsInstanceOf(Class) bool
}

type defaultInstance struct {
	class Class
	value Value
}

func (i *defaultInstance) Class() Class {
	return i.class
}

func (i *defaultInstance) Value() Value {
	return i.value
}

func (i *defaultInstance) IsInstanceOf(class Class) bool {
	if i.Class() == class {
		return true
	}
	for _, p := range i.Class().Parents() {
		if test(p, class) {
			return true
		}
	}
	return false
}
