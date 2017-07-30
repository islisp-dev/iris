package core

import (
	"github.com/ta2gch/gazelle/core/class"
	env "github.com/ta2gch/gazelle/core/environment"
)

type NativeFunction struct {
	fun func(class.Instance, *env.Environment, *env.Environment) (class.Instance, class.Instance)
}

func (f NativeFunction) Apply(args class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance) {
	obj, err := f.fun(args, local, global)
	if err != nil {
		return nil, err
	}
	return obj, nil
}

func NewNativeFunction(fun func(class.Instance, *env.Environment, *env.Environment) (class.Instance, class.Instance)) class.Instance {
	return class.New(class.Function, &NativeFunction{fun})
}
