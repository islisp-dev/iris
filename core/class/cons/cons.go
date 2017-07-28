package cons

import (
	"fmt"

	"github.com/ta2gch/gazelle/core/class"
)

// Cell is a pair of pointers to Object
type Cell struct {
	Car *class.Instance
	Cdr *class.Instance
}

func New(car *class.Instance, cdr *class.Instance) *class.Instance {
	return class.Cons.New(&Cell{car, cdr})
}

func Car(i *class.Instance) (*class.Instance, error) {
	if i == nil || i.Class() != class.Cons && i.Class() != class.List {
		return nil, fmt.Errorf("%v is not a member of <cons>", i.Class().ToString())
	}
	return i.Value().(Cell).Car, nil
}

func Cdr(i *class.Instance) (*class.Instance, error) {
	if i == nil || i.Class() != class.Cons && i.Class() != class.List {
		return nil, fmt.Errorf("%v is not a member of <cons>", i.Class().ToString())
	}
	return i.Value().(Cell).Cdr, nil
}