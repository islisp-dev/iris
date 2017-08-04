package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func tagbody(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !ilos.InstanceOf(args, class.Cons) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) < 2 { // Checked at the head of test
		return nil, instance.NewWrongNumberOfArguments(instance.NewSymbol("TAGBODY"), args)
	}
	localTags := []ilos.Instance{}
	cdr := args // Checked at the top of this function
	for ilos.InstanceOf(cdr, class.Cons) {
		cadr := instance.UnsafeCar(cdr) // Checked at the top of this loop
		cddr := instance.UnsafeCdr(cdr) // Checked at the top of this loop
		if !ilos.InstanceOf(cadr, class.Cons) {
			local.TagBodyTag.Define(cadr, cddr)
			localTags = append(localTags, cadr)
		}
		cdr = cddr
	}
	cdr = args
	for ilos.InstanceOf(cdr, class.Cons) {
		cadr := instance.UnsafeCar(cdr) // Checked at the top of this loop
		cddr := instance.UnsafeCdr(cdr) // Checked at the top of this loop
		if ilos.InstanceOf(cadr, class.Cons) {
			_, fail := Eval(cadr, local, global)
			if fail != nil {
			tag:
				if ilos.InstanceOf(fail, class.TagBodyTag) {
					tag, _ := fail.GetSlotValue(instance.NewSymbol("TAG")) // Checked at the top of this loop
					found := false
					for _, localTag := range localTags {
						if tag == localTag {
							found = true
							break
						}
					}
					if found {
						forms, _ := local.TagBodyTag.Get(tag) // Checked in the function, tagbodyGo
						cdddr := forms
						for ilos.InstanceOf(cdddr, class.Cons) {
							cadddr := instance.UnsafeCar(cdddr) // Checked at the top of this loop
							cddddr := instance.UnsafeCdr(cdddr) // Checked at the top of this loop
							if ilos.InstanceOf(cadddr, class.Cons) {
								_, fail = Eval(cadddr, local, global)
								if fail != nil {
									goto tag
								}
							}
							cdddr = cddddr
						}
						break
					}

				}
				return nil, fail
			}
		}
		cdr = cddr
	}
	return instance.NewNull(), nil
}
