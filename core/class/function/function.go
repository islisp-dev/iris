package function

import (
	"github.com/ta2gch/gazelle/core/class"
	env "github.com/ta2gch/gazelle/core/environment"
)

type Function interface {
	Apply(args class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance)
}

func Apply(fun class.Instance, args class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance) {
	if !fun.IsInstanceOf(class.Function) {
		return nil, class.New(class.DomainError, nil)
	}
	obj, err := fun.Value().(Function).Apply(args, local, global)
	if err != nil {
		return nil, err
	}
	return obj, nil
}
