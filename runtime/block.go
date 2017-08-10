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
func block(local, global *env.Environment, tag ilos.Instance, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(local, global, tag) // Checked at the top of this function
	if err != nil {
		return nil, err
	}
	if instance.Of(class.Number, tag) || instance.Of(class.Character, tag) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         tag,
			"EXPECTED-CLASS": class.Object,
		})
	}
	local.BlockTag.Define(tag, nil)
	var sucess, fail ilos.Instance
	for _, cadr := range body {
		sucess, fail = Eval(local, global, cadr)
		if fail != nil {
			if instance.Of(class.BlockTag, fail) {
				tag1, _ := fail.GetSlotValue(instance.New(class.Symbol, "TAG"), class.Escape) // Checked at the head of this condition
				if tag == tag1 {
					obj, _ := fail.GetSlotValue(instance.New(class.Symbol, "OBJECT"), class.BlockTag) // Checked at the head of this condition
					return obj, nil
				}
			}
			return nil, fail
		}
	}
	return sucess, nil
}

func return_from(local, global *env.Environment, tag, object ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(local, global, tag)
	if err != nil {
		return nil, err
	}
	if instance.Of(class.Number, tag) || instance.Of(class.Character, tag) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         tag,
			"EXPECTED-CLASS": class.Object,
		})
	}
	object, err = Eval(local, global, object)
	if err != nil {
		return nil, err
	}
	if _, ok := local.BlockTag.Get(tag); !ok {
		return nil, instance.New(class.SimpleError, map[string]ilos.Instance{
			"FORMAT-STRING":    instance.New(class.String, "%v is not defined as the tag"),
			"FORMAT-ARGUMENTS": tag,
		})
	}
	return nil, instance.New(class.BlockTag, map[string]ilos.Instance{
		"TAG":    tag,
		"OBJECT": object,
	})
}
