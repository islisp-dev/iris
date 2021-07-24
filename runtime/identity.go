package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
)

func Identity(e ilos.Environment, xs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if len(xs) != 1 {
		return SignalCondition(e, ilos.NewArityError(e), Nil)
	}
	return xs[0], nil
}
