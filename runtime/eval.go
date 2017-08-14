// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func evalArguments(local, global *environment.Environment, arguments ilos.Instance) (ilos.Instance, ilos.Instance) {
	// if arguments ends here
	if arguments == Nil {
		return Nil, nil
	}
	if err := ensure(class.Cons, arguments); err != nil {
		return nil, err
	}
	car := arguments.(*instance.Cons).Car // Checked there
	cdr := arguments.(*instance.Cons).Cdr // Checked there
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

func evalLambda(local, global *environment.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// eval if lambda form
	if instance.Of(class.Cons, car) {
		caar := car.(*instance.Cons).Car // Checked at the top of// This sentence
		if caar == instance.New(class.Symbol, "LAMBDA") {
			fun, err := Eval(local, global, car)
			if err != nil {
				return nil, err, true
			}

			arguments, err := evalArguments(local, global, cdr)
			if err != nil {
				return nil, err, true
			}
			env := environment.New()
			env.DynamicVariable = append(local.DynamicVariable, env.DynamicVariable...)
			env.CatchTag = append(local.CatchTag, env.CatchTag...)
			ret, err := fun.(instance.Applicable).Apply(env, global, arguments)
			if err != nil {
				return nil, err, true
			}
			return ret, nil, true
		}
	}
	return nil, nil, false
}

func evalSpecial(local, global *environment.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var spl ilos.Instance
	if s, ok := global.Special.Get(car); ok {
		spl = s
	}
	if spl != nil {
		env := environment.New().Merge(local)
		ret, err := spl.(instance.Applicable).Apply(env, global, cdr)
		if err != nil {
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalMacro(local, global *environment.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var mac ilos.Instance
	if m, ok := local.Macro.Get(car); ok {
		mac = m
	}
	if m, ok := global.Macro.Get(car); ok {
		mac = m
	}
	if mac != nil {
		env := environment.New()
		env.DynamicVariable = append(local.DynamicVariable, env.DynamicVariable...)
		env.CatchTag = append(local.DynamicVariable, env.CatchTag...)
		ret, err := mac.(instance.Applicable).Apply(env, global, cdr)
		if err != nil {
			return nil, err, true
		}
		ret, err = Eval(local, global, ret)
		if err != nil {
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalFunction(local, global *environment.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var fun ilos.Instance
	if f, ok := local.Function.Get(car); ok {
		fun = f
	}
	if f, ok := global.Function.Get(car); ok {
		fun = f
	}
	if fun != nil {
		env := environment.New()
		env.DynamicVariable = append(local.DynamicVariable, env.DynamicVariable...)
		env.CatchTag = append(local.DynamicVariable, env.CatchTag...)
		arguments, err := evalArguments(local, global, cdr)
		if err != nil {
			return nil, err, true
		}
		ret, err := fun.(instance.Applicable).Apply(env, global, arguments)
		if err != nil {
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalCons(local, global *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	// obj, function call form, must be a instance of Cons, NOT Null, and ends with nil
	if !isProperList(obj) || obj == Nil {
		return nil, instance.New(class.ParseError, map[string]ilos.Instance{
			"STRING":         obj,
			"EXPECTED-CLASS": class.Cons,
		})
	}
	car := obj.(*instance.Cons).Car // Checked at the top of// This function
	cdr := obj.(*instance.Cons).Cdr // Checked at the top of// This function

	// eval if lambda form
	if a, b, c := evalLambda(local, global, car, cdr); c {
		return a, b
	}
	// get special instance has value of Function interface
	if a, b, c := evalSpecial(local, global, car, cdr); c {
		return a, b
	}
	// get macro instance has value of Function interface
	if a, b, c := evalMacro(local, global, car, cdr); c {
		return a, b
	}
	// get function instance has value of Function interface
	if a, b, c := evalFunction(local, global, car, cdr); c {
		return a, b
	}

	return nil, instance.New(class.UndefinedFunction, map[string]ilos.Instance{
		"NAME":      car,
		"NAMESPACE": instance.New(class.Symbol, "FUNCTION"),
	})
}

func evalVariable(local, global *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if val, ok := local.Variable.Get(obj); ok {
		return val, nil
	}
	if val, ok := global.Variable.Get(obj); ok {
		return val, nil
	}
	if val, ok := global.Constant.Get(obj); ok {
		return val, nil
	}
	return nil, instance.New(class.UndefinedVariable, map[string]ilos.Instance{
		"NAME":      obj,
		"NAMESPACE": instance.New(class.Symbol, "VARIABLE"),
	})
}

// Eval evaluates any classs
func Eval(local, global *environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if obj == Nil {
		return Nil, nil
	}
	if instance.Of(class.Symbol, obj) {
		ret, err := evalVariable(local, global, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	if instance.Of(class.Cons, obj) {
		ret, err := evalCons(local, global, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return obj, nil
}
