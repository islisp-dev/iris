// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/k0kubun/pp"
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func evalArguments(local, global environment.Environment, arguments ilos.Instance) (ilos.Instance, ilos.Instance) {
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
	return instance.NewCons(a, b), nil

}

func evalLambda(local, global environment.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// eval if lambda form
	if ilos.InstanceOf(class.Cons, car) {
		caar := car.(*instance.Cons).Car // Checked at the top of// This sentence
		if caar == instance.NewSymbol("LAMBDA") {
			fun, err := Eval(local, global, car)
			if err != nil {
				return nil, err, true
			}

			arguments, err := evalArguments(local, global, cdr)
			if err != nil {
				return nil, err, true
			}
			env := NewEnvironment()
			env.MergeDynamic(local)
			ret, err := fun.(instance.Applicable).Apply(env, global, arguments.(instance.List).Slice()...)
			if err != nil {
				return nil, err, true
			}
			return ret, nil, true
		}
	}
	return nil, nil, false
}

func evalSpecial(local, global environment.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var spl ilos.Instance
	if s, ok := global.Special.Get(car); ok {
		spl = s
	}
	if spl != nil {
		env := NewEnvironment()
		env.Merge(local)
		ret, err := spl.(instance.Applicable).Apply(env, global, cdr.(instance.List).Slice()...)
		if err != nil {
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalMacro(local, global environment.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var mac ilos.Instance
	if m, ok := local.Macro.Get(car); ok {
		mac = m
	}
	if m, ok := global.Macro.Get(car); ok {
		mac = m
	}
	if mac != nil {
		env := NewEnvironment()
		env.MergeDynamic(local)
		ret, err := mac.(instance.Applicable).Apply(env, global, cdr.(instance.List).Slice()...)
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

func evalFunction(local, global environment.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var fun ilos.Instance
	if f, ok := global.Function.Get(car); ok {
		fun = f
	}
	if f, ok := local.Function.Get(car); ok {
		fun = f
	}
	if fun != nil {
		env := NewEnvironment()
		env.MergeDynamic(local)
		arguments, err := evalArguments(local, global, cdr)
		if err != nil {
			return nil, err, true
		}
		ret, err := fun.(instance.Applicable).Apply(env, global, arguments.(instance.List).Slice()...)
		if err != nil {
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalCons(local, global environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(class.Cons, obj); err != nil {
		return nil, err
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
	pp.Println(global, local)
	return nil, instance.NewUndefinedFunction(car)
}

func evalVariable(local, global environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if val, ok := local.Variable.Get(obj); ok {
		return val, nil
	}
	if val, ok := global.Variable.Get(obj); ok {
		return val, nil
	}
	if val, ok := global.Constant.Get(obj); ok {
		return val, nil
	}
	return nil, instance.NewUndefinedVariable(obj)
}

// Eval evaluates any classs
func Eval(local, global environment.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if obj == Nil {
		return Nil, nil
	}
	if ilos.InstanceOf(class.Symbol, obj) {
		ret, err := evalVariable(local, global, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	if ilos.InstanceOf(class.Cons, obj) {
		ret, err := evalCons(local, global, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return obj, nil
}
