package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func evalArguments(local *env.Environment, global *env.Environment, args ilos.Instance) (ilos.Instance, ilos.Instance) {
	// if args ends here
	if instance.Of(class.Null, args) {
		return instance.New(class.Null), nil
	}
	// args must be a instance of list and ends with nil
	if !instance.Of(class.List, args) || !UnsafeEndOfListIsNil(args) {
		return nil, instance.New(class.ParseError, map[string]ilos.Instance{
			"STRING":         args,
			"EXPECTED-CLASS": class.List,
		})
	}
	car := instance.UnsafeCar(args) // Checked there
	cdr := instance.UnsafeCdr(args) // Checked there
	a, err := Eval(local, global, car)
	if err != nil {
		return nil, err
	}
	b, err := evalArguments(local, global, cdr)
	if err != nil {
		return nil, err
	}
	return instance.New(class.Cons, a, b), nil

}

func evalFunction(local *env.Environment, global *env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	// obj, function call form, must be a instance of Cons, NOT Null, and ends with nil
	if !instance.Of(class.Cons, obj) || !UnsafeEndOfListIsNil(obj) {
		return nil, instance.New(class.ParseError, map[string]ilos.Instance{
			"STRING":         obj,
			"EXPECTED-CLASS": class.Cons,
		})
	}
	// get function symbol
	car := instance.UnsafeCar(obj) // Checked at the top of this function

	// get function arguments
	cdr := instance.UnsafeCdr(obj) // Checked at the top of this function

	// eval if lambda form
	if instance.Of(class.Cons, car) {
		caar := instance.UnsafeCar(car) // Checked at the top of this sentence
		if caar == instance.New(class.Symbol, "LAMBDA") {
			fun, err := Eval(local, global, car)
			if err != nil {
				return nil, err
			}

			args, err := evalArguments(local, global, cdr)
			if err != nil {
				return nil, err
			}
			env := env.New()
			env.DynamicVariable = append(local.DynamicVariable, env.DynamicVariable...)
			env.CatchTag = append(local.CatchTag, env.CatchTag...)
			ret, err := fun.(instance.Applicable).Apply(args, env, global)
			if err != nil {
				return nil, err
			}
			return ret, nil
		}
	}
	// if function is not a lambda special form, first element must be a symbol
	if !instance.Of(class.Symbol, car) {
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         car,
			"EXPECTED-CLASS": class.Symbol,
		})
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
		ret, err := mac.(instance.Applicable).Apply(cdr, local, global)
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
		args, err := evalArguments(local, global, cdr)
		if err != nil {
			return nil, err
		}
		env := env.New()
		env.DynamicVariable = append(local.DynamicVariable, env.DynamicVariable...)
		env.CatchTag = append(local.CatchTag, env.CatchTag...)
		ret, err := fun.(instance.Applicable).Apply(args, env, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return nil, instance.New(class.UndefinedFunction, map[string]ilos.Instance{
		"NAME":      car,
		"NAMESPACE": instance.New(class.Symbol, "FUNCTION"),
	})
}

// Eval evaluates any classs
func Eval(local *env.Environment, global *env.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if instance.Of(class.Null, obj) {
		return instance.New(class.Null), nil
	}
	if instance.Of(class.Symbol, obj) {
		if val, ok := local.Variable.Get(obj); ok {
			return val, nil
		}
		if val, ok := global.Variable.Get(obj); ok {
			return val, nil
		}
		return nil, instance.New(class.UndefinedVariable, map[string]ilos.Instance{
			"NAME":      obj,
			"NAMESPACE": instance.New(class.Symbol, "VARIABLE"),
		})
	}
	if instance.Of(class.Cons, obj) {
		ret, err := evalFunction(local, global, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return obj, nil
}
