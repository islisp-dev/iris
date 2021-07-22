package runtime

import (
	"github.com/islisp-dev/iris/runtime/env"
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func Identity(e env.Environment, xs ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	if len(xs) != 1 {
		return SignalCondition(e, instance.NewArityError(e), Nil)
	}
	return xs[0], nil
}
