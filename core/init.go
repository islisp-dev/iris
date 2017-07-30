package core

import (
	"github.com/ta2gch/gazelle/core/class"
	"github.com/ta2gch/gazelle/core/class/cons"
	env "github.com/ta2gch/gazelle/core/environment"
)

func init() {
	env.TopLevel.SetFunction(class.New(class.Symbol, "functionp"), NewNativeFunction(func(args class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance) {
		len, err := cons.Length(args)
		if err != nil {
			return nil, err
		}
		if len != 1 {
			return nil, class.New(class.UndefinedFunction, nil)
		}
		car, err := cons.Car(args)
		if err != nil {
			return nil, err
		}
		if car.Class() == class.Function {
			return class.New(class.Object, true), nil
		}
		return class.New(class.Null, nil), nil
	}))
}
