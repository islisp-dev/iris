package main

type Object struct {
	Type string
	Car  *Object
	Cdr  *Object
	Val  interface{}
}

func NewCons(car *Object, cdr *Object) *Object {
	return &Object{"cons", car, cdr, nil}
}
