package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func quote(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !instance.Of(class.Cons, args) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) != 1 { // Checked at the head of test
		return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      instance.New(class.Symbol, "QUOTE"),
			"ARGUMENTS": args,
		})
	}
	return instance.UnsafeCar(args), nil
}
