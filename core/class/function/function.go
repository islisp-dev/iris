package function

import (
	"fmt"

	"github.com/ta2gch/gazelle/core/class"
	env "github.com/ta2gch/gazelle/core/environment"
)

type Function interface {
	apply(args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error)
}

func Apply(fun *class.Instance, args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
	if fun.Class() != class.Function {
		return nil, fmt.Errorf("%v is not <function>", fun.Class().ToString())
	}
	obj, err := fun.Value().(Function).apply(args, local, global)
	if err != nil {
		return nil, err
	}
	return obj, nil
}

type NativeFunction struct {
	fun func(*class.Instance, *env.Environment, *env.Environment) (*class.Instance, error)
}

func (f NativeFunction) apply(args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
	obj, err := f.fun(args, local, global)
	if err != nil {
		return nil, err
	}
	return obj, nil
}

func New(fun func(*class.Instance, *env.Environment, *env.Environment) (*class.Instance, error)) *class.Instance {
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
}
*/

type GenericFunction struct {
	args *class.Instance
	body *class.Instance
}
