package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func function(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !ilos.InstanceOf(args, class.Cons) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) != 1 { // Checked at the head of test
		return nil, instance.NewParseError(args, class.Cons)
	}
	car := instance.UnsafeCar(args) // Checked at the top of this function
	// car must be a symbol
	if !ilos.InstanceOf(car, class.Symbol) {
		return nil, instance.NewDomainError(car, class.Symbol)
	}
	if f, ok := local.Function.Get(car); ok {
		return f, nil
	}
	if f, ok := global.Function.Get(car); ok {
		return f, nil
	}
	return nil, instance.NewUndefinedEntityError(nil, nil)
}
