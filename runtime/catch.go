package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func catch(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !instance.Of(class.Cons, args) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) < 2 { // Checked at the head of test
		return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      instance.New(class.Symbol, "CATCH"),
			"ARGUMENTS": args,
		})
	}
	car, err := Eval(instance.UnsafeCar(args), local, global) // Checked at the top of this function
	if err != nil {
		return nil, err
	}
	if instance.Of(class.Number, car) || instance.Of(class.Character, car) {
		return nil, instance.New(class.DomainError, car, class.Object)
	}
	local.ThrowTag.Define(car, car)
	cdr := instance.UnsafeCdr(args) // Checked at the top of this function
	var sucess, fail ilos.Instance
	for instance.Of(class.Cons, cdr) {
		cadr := instance.UnsafeCar(cdr) // Checked at the top of this loop
		cddr := instance.UnsafeCdr(cdr) // Checked at the top of this loop
		sucess, fail = Eval(cadr, local, global)
		if fail != nil {
			if instance.Of(class.CatchTag, fail) {
				tag, _ := fail.GetSlotValue(instance.New(class.Symbol, "TAG"), class.Escape) // Checked at the head of this condition
				if car == tag {
					obj, _ := fail.GetSlotValue(instance.New(class.Symbol, "OBJECT"), class.CatchTag) // Checked at the head of this condition
					return obj, nil
				}
			}
			return nil, fail
		}
		cdr = cddr
	}
	return sucess, nil
}

func throw(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !instance.Of(class.Cons, args) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) != 2 { // Checked at the head of test
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
	if instance.Of(class.Number, tag) || instance.Of(class.Character, tag) {
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
