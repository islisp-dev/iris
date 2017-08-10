package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func catch(local, global *env.Environment, tag ilos.Instance, body ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	var err ilos.Instance
	tag, err = Eval(local, global, tag)
	if err != nil {
		return nil, err
	}
	if instance.Of(class.Number, tag) || instance.Of(class.Character, tag) {
		return nil, instance.New(class.DomainError, tag, class.Object)
	}
	local.CatchTag.Define(tag, nil)
	var sucess, fail ilos.Instance
	for _, cadr := range body {
		sucess, fail = Eval(local, global, cadr)
		if fail != nil {
			if instance.Of(class.CatchTag, fail) {
				tag1, _ := fail.GetSlotValue(instance.New(class.Symbol, "TAG"), class.Escape) // Checked at the head of this condition
				if tag == tag1 {
					obj, _ := fail.GetSlotValue(instance.New(class.Symbol, "OBJECT"), class.CatchTag) // Checked at the head of this condition
					return obj, nil
				}
			}
			return nil, fail
		}
	}
	return sucess, nil
}

func throw(local, global *env.Environment, tag, object ilos.Instance) (ilos.Instance, ilos.Instance) {
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
	if _, ok := local.CatchTag.Get(tag); !ok {
		return nil, instance.New(class.SimpleError, map[string]ilos.Instance{
			"FORMAT-STRING":    instance.New(class.String, "%v is not defined as the tag"),
			"FORMAT-ARGUMENTS": tag,
		})
	}
	return nil, instance.New(class.CatchTag, map[string]ilos.Instance{
		"TAG":    tag,
		"OBJECT": object,
	})
}
