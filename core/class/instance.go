package class

type Value interface{}

// Instance struct type is the struct for the internal representations
type Instance struct {
	class *Class
	value Value
}

func (i *Instance) Class() *Class {
	return i.class
}

func (i *Instance) Value() Value {
	if i.Class() == Null {
		return nil
	}
	return i.value
}
