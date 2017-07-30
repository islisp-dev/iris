package domainerror

import (
	"fmt"

	"github.com/ta2gch/gazelle/runtime/class"
)

type domainerror struct {
	object        class.Instance
	expectedClass class.Class
}

func New(obj class.Instance, cls class.Class) class.Instance {
	return domainerror{obj, cls}
}

func (e domainerror) Value() class.Value {
	return e
}

func (e domainerror) Class() class.Class {
	return class.DomainError
}

func (e domainerror) IsInstanceOf(cls class.Class) bool {
	return class.IsInstanceOf(e, cls)
}

func (e domainerror) String() string {
	return fmt.Sprintf("Domain Error: %v is not a instance of %v", e.object, e.expectedClass)
}
