package function

import (
	"fmt"

	"github.com/ta2gch/gazelle/core/class"
	"github.com/ta2gch/gazelle/core/environment"
)

type Function interface {
	apply(args *class.Instance, global *environment.Env) (*class.Instance, error)
}

func Apply(fun *class.Instance, args *class.Instance, global *environment.Env) (*class.Instance, error) {
	if fun.Class() != class.Function {
		return nil, fmt.Errorf("%v is not <function>", fun.Class().ToString())
	}
	obj, err := fun.Value().(Function).apply(args, global)
	if err != nil {
		return nil, err
	}
	return obj, nil
}

type NativeFunction struct {
	fun func(*class.Instance, *environment.Env) (*class.Instance, error)
}

func (f NativeFunction) apply(args *class.Instance, global *environment.Env) (*class.Instance, error) {
	obj, err := f.fun(args, global)
	if err != nil {
		return nil, err
	}
	return obj, nil
}

func New(fun func(*class.Instance, *environment.Env) (*class.Instance, error)) *class.Instance {
	return class.Function.New(&NativeFunction{fun})
}

type DefaultFunction struct {
	args *class.Instance
	body *class.Instance
}

/*
func (f *DefaultFunction) Apply(args *class.Instance, global *Env) (*class.Instance, error) {
	// Assign args to local environment
	// Eval each body
	// return evaluate result
}z
*/
