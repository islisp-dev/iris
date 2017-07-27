package object

import (
	"github.com/ta2gch/gazelle/runtime/class"
)

// Object struct type is the struct for the internal representations
type Object struct {
	Class *class.Class
	Val   interface{}
}

// Cons is a pair of pointers to Object
type Cons struct {
	Car *Object
	Cdr *Object
}
