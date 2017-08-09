package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

// DEFINISION:
// The block special form executes each form sequentially from left to right.
// If the last form exits normally, whatever it returns is returned by the block form.
// The name in a block form is not evaluated; it must be an identifier. The scope of name
// is the body form only a return-from textually contained in some form can exit the block.
// The extend of name is dynamic. (islisp-v23.pdf, 43-44)
// NOTE:
// According to this, the scope of name is dynamic. I guess it should be a static.
func block(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !ilos.InstanceOf(args, class.Cons) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) < 2 { // Checked at the head of test
		return nil, instance.NewWrongNumberOfArguments(instance.NewSymbol("BLOCK"), args)
	}
	car, err := Eval(instance.UnsafeCar(args), local, global) // Checked at the top of this function
	if err != nil {
		return nil, err
	}
	if ilos.InstanceOf(car, class.Number) || ilos.InstanceOf(car, class.Character) {
		return nil, instance.NewDomainError(car, class.Object)
	}
	local.BlockTag.Define(car, car)
	cdr := instance.UnsafeCdr(args) // Checked at the top of this function
	var sucess, fail ilos.Instance
	for ilos.InstanceOf(cdr, class.Cons) {
		cadr := instance.UnsafeCar(cdr) // Checked at the top of this loop
		cddr := instance.UnsafeCdr(cdr) // Checked at the top of this loop
		sucess, fail = Eval(cadr, local, global)
		if fail != nil {
			if ilos.InstanceOf(fail, class.BlockTag) {
				tag, _ := fail.GetSlotValue(instance.NewSymbol("TAG"), class.BlockTag) // Checked at the head of this condition
				if car == tag {
					obj, _ := fail.GetSlotValue(instance.NewSymbol("OBJECT"), class.BlockTag) // Checked at the head of this condition
					return obj, nil
				}
			}
			return nil, fail
		}
		cdr = cddr
	}
	return sucess, nil
}
