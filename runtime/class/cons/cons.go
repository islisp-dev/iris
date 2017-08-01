package cons

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/class"
	"github.com/ta2gch/iris/runtime/class/domainerror"
)

// Cell is a pair of pointers to Object
type cons struct {
	car class.Instance
	cdr class.Instance
}

func New(car class.Instance, cdr class.Instance) class.Instance {
	return cons{car, cdr}
}

func (c cons) Class() class.Class {
	return class.Cons
}

func (c cons) Value() class.Value {
	return c
}

func (c cons) IsInstanceOf(cls class.Class) bool {
	return class.IsInstanceOf(c, cls)
}

func (c cons) String() string {
	return fmt.Sprintf("(%v . %v)", c.car, c.cdr)
}

func Car(i class.Instance) (class.Instance, class.Instance) {
	if i.IsInstanceOf(class.Null) || !i.IsInstanceOf(class.List) {
		return nil, domainerror.New(i, class.Cons)
	}
	return i.(cons).car, nil
}

func Cdr(i class.Instance) (class.Instance, class.Instance) {
	if i.IsInstanceOf(class.Null) || !i.IsInstanceOf(class.List) {
		return nil, domainerror.New(i, class.Cons)
	}
	return i.(cons).cdr, nil
}

func Length(list class.Instance) (int, class.Instance) {
	if !list.IsInstanceOf(class.List) {
		return 0, domainerror.New(list, class.Cons)
	}
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
