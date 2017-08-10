package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func tagbody(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !instance.Of(class.Cons, args) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) < 2 { // Checked at the head of test
		return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      instance.New(class.Symbol, "TAGBODY"),
			"ARGUMENTS": args,
		})
	}
	localTags := []ilos.Instance{}
	cdr := args // Checked at the top of this function
	for instance.Of(class.Cons, cdr) {
		cadr := instance.UnsafeCar(cdr) // Checked at the top of this loop
		cddr := instance.UnsafeCdr(cdr) // Checked at the top of this loop
		if !instance.Of(class.Cons, cadr) {
			local.TagbodyTag.Define(cadr, nil)
			localTags = append(localTags, cadr)
		}
		cdr = cddr
	}
	cdr = args
	for instance.Of(class.Cons, cdr) {
		cadr := instance.UnsafeCar(cdr) // Checked at the top of this loop
		cddr := instance.UnsafeCdr(cdr) // Checked at the top of this loop
		if instance.Of(class.Cons, cadr) {
			_, fail := Eval(cadr, local, global)
			if fail != nil {
			tag:
				if instance.Of(class.TagbodyTag, fail) {
					tag, _ := fail.GetSlotValue(instance.New(class.Symbol, "TAG"), class.Escape) // Checked at the top of this loop
					found := false
					for _, localTag := range localTags {
						if tag == localTag {
							found = true
							break
						}
					}
					if found {
						forms, _ := local.TagbodyTag.Get(tag) // Checked in the function, tagbodyGo
						cdddr := forms
						for instance.Of(class.Cons, cdddr) {
							cadddr := instance.UnsafeCar(cdddr) // Checked at the top of this loop
							cddddr := instance.UnsafeCdr(cdddr) // Checked at the top of this loop
							if instance.Of(class.Cons, cadddr) {
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
	return instance.New(class.Null), nil
}

func tagbodyGo(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !instance.Of(class.Cons, args) || !UnsafeEndOfListIsNil(args) || UnsafeListLength(args) != 1 { // Checked at the head of test
		return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      instance.New(class.Symbol, "GO"),
			"ARGUMENTS": args,
		})
	}
	car := instance.UnsafeCar(args) // Checked at the top of this function
	if _, ok := local.TagbodyTag.Get(car); !ok {
		return nil, instance.New(class.SimpleError, map[string]ilos.Instance{
			"FORMAT-STRING":    instance.New(class.String, "%v is not defined as the tag"),
			"FORMAT-ARGUMENTS": car,
		})
	}
	return nil, instance.New(class.TagbodyTag, map[string]ilos.Instance{"TAG": car})
}
