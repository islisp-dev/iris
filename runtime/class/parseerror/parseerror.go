package parseerror

import (
	"fmt"

	"github.com/ta2gch/gazelle/runtime/class"
)

type parseerror struct {
	str           string
	expectedClass class.Class
}

func New(str string, cls class.Class) class.Instance {
	return parseerror{str, cls}
}

func (e parseerror) Value() class.Value {
	return e
}

func (e parseerror) Class() class.Class {
	return class.DomainError
}

func (e parseerror) IsInstanceOf(cls class.Class) bool {
	return class.IsInstanceOf(e, cls)
}

func (e parseerror) String() string {
	return fmt.Sprintf("Parse Error: %v is not a instance of %v", e.str, e.expectedClass)
}
