package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func function(local, global *environment.Environment, fun ilos.Instance) (ilos.Instance, ilos.Instance) {
	// car must be a symbol
	if !instance.Of(class.Symbol, fun) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         fun,
			"EXPECTED-CLASS": class.Symbol,
		})
	}
	if f, ok := local.Function.Get(fun); ok {
		return f, nil
	}
	if f, ok := global.Function.Get(fun); ok {
		return f, nil
	}
	return nil, instance.New(class.UndefinedFunction, map[string]ilos.Instance{
		"NAME":      fun,
		"NAMESPACE": instance.New(class.Symbol, "FUNCTION"),
	})
}
