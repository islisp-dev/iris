package object

import "github.com/ta2gch/gazelle/runtime/class"

// Object struct type is the struct for the internal representations
type Object struct {
	Class *class.Class
	Car   *Object
	Cdr   *Object
	Val   interface{}
}

// Is is for testing
func (o *Object) Is(val interface{}) bool {
	return o.Val == val
}

// Copy creates a clone of Object
func (o *Object) Copy() *Object {
	if o == nil {
		return nil
	}
	return &Object{o.Class, o.Car.Copy(), o.Cdr.Copy(), o.Val}
}
