package lib

import "github.com/islisp-dev/iris/core"

func Identity(e core.Environment, xs ...core.Instance) (core.Instance, core.Instance) {
	if len(xs) != 1 {
		return SignalCondition(e, core.NewArityError(e), Nil)
	}
	return xs[0], nil
}
