package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func lambda(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	lambdaList := instance.UnsafeCar(args)
	forms := instance.UnsafeCdr(args)
	lexical := local
	return instance.NewFunction(func(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
		env.AppendAll(local, lexical)
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
				local.DefineVariable(cadr, aargs)
				break
			}
			local.DefineVariable(key, value)
			fargs = instance.UnsafeCdr(fargs)
			aargs = instance.UnsafeCdr(aargs)
		}
		body := forms
		ret := instance.NewNull()
		var err ilos.Instance
		for !ilos.InstanceOf(body, class.Null) {
			exp := instance.UnsafeCar(body)
			ret, err = Eval(exp, local, global)
			if err != nil {
				return nil, err
			}
			body = instance.UnsafeCdr(body)
		}
		return ret, nil
	}), nil
}
