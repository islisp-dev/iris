package class

// Instance struct type is the struct for the internal representations
type Instance struct {
	class *Class
	value interface{}
}

func (i *Instance) Class() *Class {
	if i == nil {
		return Null
	}
	return i.class
}

func (i *Instance) Value() interface{} {
	if i == nil {
		return Null
	}
	return i.value
}
