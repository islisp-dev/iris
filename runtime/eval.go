package runtime

import (
	"github.com/ta2gch/gazelle/runtime/class"
	"github.com/ta2gch/gazelle/runtime/class/cons"
	"github.com/ta2gch/gazelle/runtime/class/function"
	env "github.com/ta2gch/gazelle/runtime/environment"
)

func evalArguments(args class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance) {
	if args.IsInstanceOf(class.Null) {
		return class.Null.New(nil), nil
	}
	car, err := cons.Car(args)
	if err != nil {
		return nil, err
	}
	cdr, err := cons.Cdr(args)
	if err != nil {
		return nil, err
	}
	a, err := Eval(car, local, global)
	if err != nil {
		return nil, err
	}
	b, err := evalArguments(cdr, local, global)
	if err != nil {
		return nil, err
	}
	return cons.New(a, b), nil
}

func evalFunction(obj class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance) {
	// get function symbol
	car, err := cons.Car(obj)
	if err != nil {
		return nil, err
	}

	// get function arguments
	cdr, err := cons.Cdr(obj)
	if err != nil {
		return nil, err
	}
	// eval if lambda form
	if car.IsInstanceOf(class.Cons) {
		caar, err := cons.Car(car)
		if err != nil {
			return nil, err
		}
		if caar.Value() == "lambda" {
			fun, err := Eval(car, local, global)
			if err != nil {
				return nil, err
			}
			args, err := evalArguments(cdr, local, global)
			if err != nil {
				return nil, err
			}
			env := env.New()
			env.MergeDynamicVariable(local)
			ret, err := function.Apply(fun, args, env, global)
			if err != nil {
				return nil, err
			}
			return ret, nil
		}
	}

	if !car.IsInstanceOf(class.Symbol) {
		return nil, class.New(class.DomainError, nil)
	}
	// get macro instance has value of Function interface
	var mac class.Instance
	if m, ok := local.GetMacro(car); ok {
		mac = m
	}
	if m, ok := global.GetMacro(car); ok {
		mac = m
	}
	if mac != nil {
		ret, err := function.Apply(mac, cdr, local, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	// get function instance has value of Function interface
	var fun class.Instance
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
		env := env.New()
		env.MergeDynamicVariable(local)
		ret, err := function.Apply(fun, args, env, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return nil, class.New(class.UndefinedFunction, nil)
}

// Eval evaluates any classs
func Eval(obj class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance) {
	if obj.IsInstanceOf(class.Null) {
		return class.Null.New(nil), nil
	}
	if obj.IsInstanceOf(class.Symbol) {
		if val, ok := local.GetVariable(obj); ok {
			return val, nil
		}
		if val, ok := global.GetVariable(obj); ok {
			return val, nil
		}
		return nil, class.New(class.UndefinedVariable, nil)
	}
	if obj.IsInstanceOf(class.Cons) {
		ret, err := evalFunction(obj, local, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return obj, nil
}
