package class

type Value interface{}

// Instance struct type is the struct for the internal representations
type Instance interface {
	Class() Instance
	Value() Value
}

type DefaultInstance struct {
	class Instance
	value Value
}

func (i *DefaultInstance) Class() Instance {
	return i.class
}

func (i *DefaultInstance) Value() Value {
	if i.Class() == nil {
		return nil
	}
	return i.value
}
