package runtime

import (
	"github.com/ta2gch/gazelle/runtime/class"
)

type Function interface {
	Apply(args *class.Instance, global *Env) (*class.Instance, error)
}

type NativeFunction struct {
	fun func(*class.Instance, *Env) (*class.Instance, error)
}

func (f NativeFunction) Apply(args *class.Instance, global *Env) (*class.Instance, error) {
	obj, err := f.fun(args, global)
	return obj, err
}
