package main

import "errors"

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

// CopyList create a clone list
func CopyList(list *Object) (*Object, error) {
	if list == nil {
		return nil, nil
	}
	if list.Type != "list" {
		return nil, errors.New("not a list")
	}
	cdr, err := CopyList(list.Cdr)
	if err != nil {
		return nil, err
	}
	return NewCons(list.Car, cdr), nil
}
