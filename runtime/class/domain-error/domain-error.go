package domainerror

import (
	"github.com/ta2gch/gazelle/runtime/class"
)

type DomainError struct {
	object        class.Instance
	expectedClass class.Instance
}
