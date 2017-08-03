package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func NewLambdaFunction(lambdaList ilos.Instance, forms ilos.Instance, lexical *env.Environment) ilos.Instance {
	return instance.NewFunction(func(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
		local.MergeAll(lexical)
		fargs := lambdaList
		aargs := args
		for !ilos.InstanceOf(fargs, class.Null) {
			key := instance.UnsafeCar(fargs)
			value := instance.UnsafeCar(aargs)
			if key == instance.NewSymbol(":rest") || key == instance.NewSymbol("&rest") {
				cdr := instance.UnsafeCdr(fargs)
				cadr := instance.UnsafeCar(cdr)
				cddr := instance.UnsafeCdr(cdr)
				if !ilos.InstanceOf(cddr, class.Null) {
					return nil, instance.NewParseError(instance.NewString(fargs.String()), class.List)
				}
				local.SetVariable(cadr, aargs)
				break
			}
			local.SetVariable(key, value)
			fargs = instance.UnsafeCdr(fargs)
			aargs = instance.UnsafeCdr(aargs)
		}
		body := forms
		var ret, err ilos.Instance
		for !ilos.InstanceOf(body, class.Null) {
			exp := instance.UnsafeCar(body)
			ret, err = Eval(exp, local, global)
			if err != nil {
				return nil, err
			}
			body = instance.UnsafeCdr(body)
		}
		return ret, nil
	})
}
