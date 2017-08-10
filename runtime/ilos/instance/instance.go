package instance

import (
	"github.com/k0kubun/pp"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
)

//
// instance
//

func New(c ilos.Class, s ...interface{}) ilos.Instance {
	switch c {
	case class.Integer:
		return Integer(s[0].(int))
	case class.Float:
		return Float(s[0].(float64))
	case class.String:
		return String(s[0].(string))
	case class.Symbol:
		return Symbol(s[0].(string))
	case class.Character:
		return Character(s[0].(rune))
	case class.Function:
		if len(s) == 4 {
			return Function{s[0].(Symbol), s[1], s[2].(int), s[3].(int)}
		}
		return NativeFunction{s[0].(Symbol), s[1]}
	case class.Cons:
		return &Cons{s[0].(ilos.Instance), s[1].(ilos.Instance)}
	case class.Null:
		return Null{}
	case class.GeneralVector:
		return GeneralVector(s[0].([]ilos.Instance))
	case class.String:
		return String(s[0].(string))
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

func Of(p ilos.Class, i ilos.Instance) bool {
	if i.Class() == p {
		return true
	}
	return class.Is(i.Class(), p)
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
	return pp.Sprint(i)
}
