package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// Not is the logical “not” (or “¬”). It returns t if obj is nil
// and nil otherwise. obj may be any ISLISP object.
func Not(_, _ *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Null, obj) {
		return T, nil
	}
	return Nil, nil
}
