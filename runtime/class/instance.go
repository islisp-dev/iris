package class

import (
	"fmt"
)

type Value interface{}

// Instance struct type is the struct for the internal representations
type Instance interface {
	Class() Class
	Value() Value
	IsInstanceOf(Class) bool
	String() string
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
	return isInstanceOf(i, class)
}

func (i *defaultInstance) String() string {
	return fmt.Sprintf("%v", i.Value())
}
