package main

// Object struct type is the struct for the internal representations
type Object struct {
	Type string
	Car  *Object
	Cdr  *Object
	Val  interface{}
}

// NewCons creates cons pair from two objects
func NewCons(car *Object, cdr *Object) *Object {
	if cdr == nil || cdr.Type == "list" {
		return &Object{"list", car, cdr, nil}
	}
	return &Object{"cons", car, cdr, nil}
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
	return &Object{o.Type, o.Car.Copy(), o.Cdr.Copy(), o.Val}
}
