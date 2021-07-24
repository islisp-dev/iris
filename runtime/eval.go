// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"github.com/islisp-dev/iris/runtime/ilos"
	"github.com/islisp-dev/iris/runtime/ilos/instance"
)

func evalArguments(e ilos.Environment, arguments ilos.Instance) (ilos.Instance, ilos.Instance) {
	// if arguments ends here
	if arguments == Nil {
		return Nil, nil
	}
	if err := ensure(e, instance.ConsClass, arguments); err != nil {
		return nil, err
	}
	car := arguments.(*instance.Cons).Car // Checked there
	cdr := arguments.(*instance.Cons).Cdr // Checked there
	a, err := Eval(e, car)
	if err != nil {
		return nil, err
	}
	b, err := evalArguments(e, cdr)
	if err != nil {
		return nil, err
	}
	return instance.NewCons(a, b), nil

}

func evalLambda(e ilos.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// eval if lambda form
	if ilos.InstanceOf(instance.ConsClass, car) {
		caar := car.(*instance.Cons).Car // Checked at the top of// This sentence
		if caar == instance.NewSymbol("LAMBDA") {
			fun, err := Eval(e, car)
			if err != nil {
				return nil, err, true
			}

			arguments, err := evalArguments(e, cdr)
			if err != nil {
				return nil, err, true
			}
			ret, err := fun.(instance.Applicable).Apply(e.NewDynamic(), arguments.(instance.List).Slice()...)
			if err != nil {
				return nil, err, true
			}
			return ret, nil, true
		}
	}
	return nil, nil, false
}

func evalSpecial(e ilos.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var spl ilos.Instance
	if s, ok := e.Special.Get(car); ok {
		spl = s
	}
	if spl != nil {
		ret, err := spl.(instance.Applicable).Apply(e.NewLexical(), cdr.(instance.List).Slice()...)
		if err != nil {
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalMacro(e ilos.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var mac ilos.Instance
	if m, ok := e.Macro.Get(car); ok {
		mac = m
	}
	if mac != nil {
		ret, err := mac.(instance.Applicable).Apply(e.NewDynamic(), cdr.(instance.List).Slice()...)
		if err != nil {
			return nil, err, true
		}
		ret, err = Eval(e, ret)
		if err != nil {
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalFunction(e ilos.Environment, car, cdr ilos.Instance) (ilos.Instance, ilos.Instance, bool) {
	// get special instance has value of Function interface
	var fun ilos.Instance
	if f, ok := e.Function.Get(car); ok {
		fun = f
	}
	if fun != nil {
		arguments, err := evalArguments(e, cdr)
		if err != nil {
			return nil, err, true
		}
		ret, err := fun.(instance.Applicable).Apply(e.NewDynamic(), arguments.(instance.List).Slice()...)
		if err != nil {
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalCons(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if err := ensure(e, instance.ConsClass, obj); err != nil {
		return nil, err
	}
	car := obj.(*instance.Cons).Car // Checked at the top of// This function
	cdr := obj.(*instance.Cons).Cdr // Checked at the top of// This function

	// eval if lambda form
	if a, b, c := evalLambda(e, car, cdr); c {
		return a, b
	}
	// get special instance has value of Function interface
	if a, b, c := evalSpecial(e, car, cdr); c {
		return a, b
	}
	// get macro instance has value of Function interface
	if a, b, c := evalMacro(e, car, cdr); c {
		return a, b
	}
	// get function instance has value of Function interface
	if a, b, c := evalFunction(e, car, cdr); c {
		return a, b
	}
	return SignalCondition(e, instance.NewUndefinedFunction(e, car), Nil)
}

func evalVariable(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if val, ok := e.Variable.Get(obj); ok {
		return val, nil
	}
	if val, ok := e.Constant.Get(obj); ok {
		return val, nil
	}
	return SignalCondition(e, instance.NewUndefinedVariable(e, obj), Nil)
}

// Eval evaluates any classs
func Eval(e ilos.Environment, obj ilos.Instance) (ilos.Instance, ilos.Instance) {
	if obj == Nil {
		return Nil, nil
	}
	if ilos.InstanceOf(instance.SymbolClass, obj) {
		ret, err := evalVariable(e, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	if ilos.InstanceOf(instance.ConsClass, obj) {
		ret, err := evalCons(e, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return obj, nil
}
