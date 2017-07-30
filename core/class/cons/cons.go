package cons

import (
	"github.com/ta2gch/gazelle/core/class"
)

// Cell is a pair of pointers to Object
type Cell struct {
	car class.Instance
	cdr class.Instance
}

func New(car class.Instance, cdr class.Instance) class.Instance {
	return class.New(class.Cons, Cell{car, cdr})
}

func Car(i class.Instance) (class.Instance, class.Instance) {
	if !i.IsInstanceOf(class.List) {
		return nil, class.New(class.DomainError, nil)
	}
	return i.Value().(Cell).car, nil
}

func Cdr(i class.Instance) (class.Instance, class.Instance) {
	if i.IsInstanceOf(class.Null) || !i.IsInstanceOf(class.List) {
		return nil, class.New(class.DomainError, nil)
	}
	return i.Value().(Cell).cdr, nil
}

func Length(list class.Instance) (int, class.Instance) {
	if list.IsInstanceOf(class.Null) {
		return 0, nil
	}
	cdr, err := Cdr(list)
	if err != nil {
		return 0, err
	}
	len, err := Length(cdr)
	if err != nil {
		return 0, err
	}
	return 1 + len, nil
}
