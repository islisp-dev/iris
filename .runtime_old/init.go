package runtime

import (
	"github.com/ta2gch/iris/runtime/class"
	"github.com/ta2gch/iris/runtime/class/cons"
	env "github.com/ta2gch/iris/runtime/environment"
)

func init() {
	env.TopLevel.SetFunction(class.Symbol.New("functionp"), NewNativeFunction(func(args class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance) {
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
		if car.IsInstanceOf(class.Function) {
			return class.Object.New(nil), nil
		}
		return class.Null.New(nil), nil
	}))
}
