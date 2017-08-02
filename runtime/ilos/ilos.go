package ilos

type Class interface {
	Class() Class
	Parents() []Class
	GetSlotValue(Instance) (Instance, bool)
	SetSlotValue(Instance, Instance) bool
	String() string
}

type Instance interface {
	Class() Class
	GetSlotValue(Instance) (Instance, bool)
	SetSlotValue(Instance, Instance) bool
	String() string
}

func ChildOf(c, p Class) bool {
	var sub func(c, p Class) bool
	sub = func(c, p Class) bool {
		if c == p {
			return true
		}
		for _, d := range c.Parents() {
			if sub(d, p) {
				return true
			}
		}
		return false
	}
	for _, d := range c.Parents() {
		if sub(d, p) {
			return true
		}
	}
	return false
}

func InstanceOf(i Instance, p Class) bool {
	if i.Class() == p {
		return true
	}
	return ChildOf(i.Class(), p)
}
