package instance

import (
	"fmt"

	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// instance
//

func New(c ilos.Class, s ...interface{}) ilos.Instance {
	switch c {
	case class.Integer:
		return NewInteger(s[0].(int))
	case class.Float:
		return NewFloat(s[0].(float64))
	case class.String:
		return NewString(s[0].(string))
	case class.Symbol:
		return NewSymbol(s[0].(string))
	case class.Character:
		return NewCharacter(s[0].(rune))
	case class.Function:
		return NewFunction(s[0].(func(ilos.Instance, *environment.Environment, *environment.Environment) (ilos.Instance, ilos.Instance)))
	case class.Cons:
		return NewCons(s[0].(ilos.Instance), s[1].(ilos.Instance))
	case class.Null:
		return NewNull()
	default:
		p := []ilos.Instance{}
		for _, q := range c.Parents() {
			p = append(p, New(q, s...))
		}
		t := map[string]ilos.Instance{}
		for _, n := range c.Slots() {
			t[n] = s[0].(map[string]ilos.Instance)[n]
		}
		return &instance{c, p, t}
	}
}

type instance struct {
	class  ilos.Class
	supers []ilos.Instance
	slots  map[string]ilos.Instance
}

func (i *instance) Class() ilos.Class {
	return i.class
}

func (i *instance) GetSlotValue(key ilos.Instance, class ilos.Class) (ilos.Instance, bool) {
	if v, ok := i.slots[string(key.(Symbol))]; ok && i.class == class {
		return v, ok
	}
	for _, s := range i.supers {
		if v, ok := s.GetSlotValue(key, class); ok {
			return v, ok
		}
	}
	return nil, false
}

func (i *instance) SetSlotValue(key ilos.Instance, value ilos.Instance, class ilos.Class) bool {
	if _, ok := i.slots[string(key.(Symbol))]; ok && i.class == class {
		i.slots[string(key.(Symbol))] = value
		return true
	}
	for _, s := range i.supers {
		if ok := s.SetSlotValue(key, value, class); ok {
			return ok
		}
	}
	return false
}

func (i *instance) String() string {
	class := fmt.Sprint(i.class)
	return fmt.Sprintf("#%v %v>", class[:len(class)-1], i.slots)
}
