package cons

import (
	"github.com/ta2gch/gazelle/core/class"
)

// Cell is a pair of pointers to Object
type Cell struct {
	car *class.Instance
	cdr *class.Instance
}

func New(car *class.Instance, cdr *class.Instance) *class.Instance {
	return class.Cons.New(&Cell{car, cdr})
}

func Car(i *class.Instance) (*class.Instance, *class.Instance) {
	if i.Class() == class.Null || (i.Class() != class.Cons && i.Class() != class.List) {
		return nil, class.DomainError.New(nil)
	}
	return i.Value().(*Cell).car, nil
}

func Cdr(i *class.Instance) (*class.Instance, *class.Instance) {
	if i.Class() == class.Null || (i.Class() != class.Cons && i.Class() != class.List) {
		return nil, class.DomainError.New(nil)
	}
	return i.Value().(*Cell).cdr, nil
}
