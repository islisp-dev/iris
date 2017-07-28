package class

// Instance struct type is the struct for the internal representations
type Instance struct {
	class *Class
	value interface{}
}

func (i *Instance) Class() *Class {
	return i.class
}

func (i *Instance) Value() interface{} {
	if i.Class() == Null {
		return nil
	}
	return i.value
}
