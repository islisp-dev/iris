package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func evalArguments(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// if args ends here
	if ilos.InstanceOf(args, class.Null) {
		return instance.NewNull(), nil
	}
	// args must be a instance of list and ends with nil
	if !ilos.InstanceOf(args, class.List) || !UnsafeEndOfListIsNil(args) {
		return nil, instance.NewParseError(args, class.List)
	}
	car := instance.UnsafeCar(args) // Checked there
	cdr := instance.UnsafeCdr(args) // Checked there
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
	// obj, function call form, must be a instance of Cons, NOT Null, and ends with nil
	if !ilos.InstanceOf(obj, class.Cons) || !UnsafeEndOfListIsNil(obj) {
		return nil, instance.NewParseError(obj, class.Cons)
	}
	// get function symbol
	car := instance.UnsafeCar(obj) // Checked at the top of this function

	// get function arguments
	cdr := instance.UnsafeCdr(obj) // Checked at the top of this function

	// eval if lambda form
	if ilos.InstanceOf(car, class.Cons) {
		caar := instance.UnsafeCar(car) // Checked at the top of this sentence
		if caar == instance.NewSymbol("LAMBDA") {
			fun, err := Eval(car, local, global)
			if err != nil {
				return nil, err
			}

			args, err := evalArguments(cdr, local, global)
			if err != nil {
				return nil, err
			}
			env := env.New()
			env.DynamicVariable = append(local.DynamicVariable, env.DynamicVariable...)
			env.ThrowTag = append(local.ThrowTag, env.ThrowTag...)
			ret, err := fun.(instance.Function)(args, env, global)
			if err != nil {
				return nil, err
			}
			return ret, nil
		}
	}
	// if function is not a lambda special form, first element must be a symbol
	if !ilos.InstanceOf(car, class.Symbol) {
		return nil, instance.NewDomainError(car, class.Symbol)
	}
	// get macro instance has value of Function interface
	var mac ilos.Instance
	if m, ok := local.Macro.Get(car); ok {
		mac = m
	}
	if m, ok := global.Macro.Get(car); ok {
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
	if f, ok := local.Function.Get(car); ok {
		fun = f
	}
	if f, ok := global.Function.Get(car); ok {
		fun = f
	}
	if fun != nil {
		args, err := evalArguments(cdr, local, global)
		if err != nil {
			return nil, err
		}
		env := env.New()
		env.DynamicVariable = append(local.DynamicVariable, env.DynamicVariable...)
		env.ThrowTag = append(local.ThrowTag, env.ThrowTag...)
		ret, err := fun.(instance.Function)(args, env, global)
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
		if val, ok := local.Variable.Get(obj); ok {
			return val, nil
		}
		if val, ok := global.Variable.Get(obj); ok {
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
