package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func evalArguments(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(args, class.Null) {
		return instance.NewNull(), nil
	}
	car := instance.UnsafeCar(args)
	cdr := instance.UnsafeCdr(args)
	a, err := Eval(car, local, global)
	if err != nil {
		return nil, err
	}
	b, err := evalArguments(cdr, local, global)
	if err != nil {
		return nil, err
	}
	return instance.NewCons(a, b), nil
}

func evalFunction(obj ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// get function symbol
	car := instance.UnsafeCar(obj)

	// get function arguments
	cdr := instance.UnsafeCdr(obj)

	// eval if lambda form
	if ilos.InstanceOf(car, class.Cons) {
		caar := instance.UnsafeCar(car)
		if caar == instance.NewSymbol("LAMBDA") {
			fun, err := Eval(car, local, global)
			if err != nil {
				return nil, err
			}
			args, err := evalArguments(cdr, local, global)
			if err != nil {
				return nil, err
			}
			e := env.New()
			env.AppendDynamicVariable(e, local)
			ret, err := fun.(instance.Function)(args, e, global)
			if err != nil {
				return nil, err
			}
			return ret, nil
		}
	}

	if !ilos.InstanceOf(car, class.Symbol) {
		return nil, instance.NewDomainError(car, class.Symbol)
	}
	// get macro instance has value of Function interface
	var mac ilos.Instance
	if m, ok := local.GetMacro(car); ok {
		mac = m
	}
	if m, ok := global.GetMacro(car); ok {
		mac = m
	}
	if mac != nil {
		ret, err := mac.(instance.Function)(cdr, local, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	// get function instance has value of Function interface
	var fun ilos.Instance
	if f, ok := local.GetFunction(car); ok {
		fun = f
	}
	if f, ok := global.GetFunction(car); ok {
		fun = f
	}
	if fun != nil {
		args, err := evalArguments(cdr, local, global)
		if err != nil {
			return nil, err
		}
		e := env.New()
		env.AppendDynamicVariable(e, local)
		ret, err := fun.(instance.Function)(args, e, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return nil, instance.NewUndefinedEntityError(nil, nil)
}

// Eval evaluates any classs
func Eval(obj ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	if ilos.InstanceOf(obj, class.Null) {
		return instance.NewNull(), nil
	}
	if ilos.InstanceOf(obj, class.Symbol) {
		if val, ok := local.GetVariable(obj); ok {
			return val, nil
		}
		if val, ok := global.GetVariable(obj); ok {
			return val, nil
		}
		return nil, instance.NewUndefinedEntityError(nil, nil)
	}
	if ilos.InstanceOf(obj, class.Cons) {
		ret, err := evalFunction(obj, local, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return obj, nil
}
