package class

type Class interface {
	// For Instance interface
	Class() Class
	Value() Value
	IsInstanceOf(Class) bool
	String() string
	// Class main interface
	Parents() []Class
	New(...Value) Instance
}

// Test function for IsInstanceOf
func test(child Class, parent Class) bool {
	if child == parent {
		return true
	}
	for _, p := range child.Parents() {
		if test(p, parent) {
			return true
		}
	}
	return false
}

func IsInstanceOf(i Instance, class Class) bool {
	if i.Class() == class {
		return true
	}
	for _, p := range i.Class().Parents() {
		if test(p, class) {
			return true
		}
	}
	return false
}

func New(class Class, value ...Value) Instance {
	return class.New(value...)
}
