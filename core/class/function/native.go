package function

import (
	"github.com/ta2gch/gazelle/core/class"
	env "github.com/ta2gch/gazelle/core/environment"
)

type NativeFunction struct {
	fun func(*class.Instance, *env.Environment, *env.Environment) (*class.Instance, error)
}

func (f NativeFunction) Apply(args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
	obj, err := f.fun(args, local, global)
	if err != nil {
		return nil, err
	}
	return obj, nil
}

func New(fun func(*class.Instance, *env.Environment, *env.Environment) (*class.Instance, error)) *class.Instance {
	return class.Function.New(&NativeFunction{fun})
}
