package main

type Object struct {
	Type string
	Car  *Object
	Cdr  *Object
	Val  interface{}
}

func NewCons(car *Object, cdr *Object) *Object {
	if cdr == nil || cdr.Type == "list" {
		return &Object{"list", car, cdr, nil}
	}
	return &Object{"cons", car, cdr, nil}
}

func (o *Object) Is(val interface{}) bool {
	return o.Val == val
}
