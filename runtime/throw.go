package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func throw(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !ilos.InstanceOf(args, class.Cons) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) != 2 { // Checked at the head of test
		return nil, instance.NewWrongNumberOfArguments(instance.NewSymbol("THROW"), args)
	}
	car := instance.UnsafeCar(args) // Checked at the top of this function
	tag, err := Eval(car, local, global)
	if err != nil {
		return nil, err
	}
	cadr := instance.UnsafeCar(instance.UnsafeCdr(args)) // Checked length is 2 at the top of this function
	object, err := Eval(cadr, local, global)
	if err != nil {
		return nil, err
	}
	if _, ok := local.ThrowTag.Get(tag); !ok {
		return nil, instance.NewSimpleError(instance.NewString("%v is not defined as the tag"), car)
	}
	return nil, instance.NewThrow(tag, object)
}
