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
		return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      instance.New(class.Symbol, "THROW"),
			"ARGUMENTS": args,
		})
	}
	car := instance.UnsafeCar(args) // Checked at the top of this function
	tag, err := Eval(car, local, global)
	if err != nil {
		return nil, err
	}
	if ilos.InstanceOf(tag, class.Number) || ilos.InstanceOf(tag, class.Character) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         tag,
			"EXPECTED-CLASS": class.Object,
		})
	}
	cadr := instance.UnsafeCar(instance.UnsafeCdr(args)) // Checked length is 2 at the top of this function
	object, err := Eval(cadr, local, global)
	if err != nil {
		return nil, err
	}
	if _, ok := local.ThrowTag.Get(tag); !ok {
		return nil, instance.New(class.SimpleError, map[string]ilos.Instance{
			"FORMAT-STRING":    instance.New(class.String, "%v is not defined as the tag"),
			"FORMAT-ARGUMENTS": car,
		})
	}
	return nil, instance.New(class.CatchTag, map[string]ilos.Instance{
		"TAG":    tag,
		"OBJECT": object,
	})
}
