package core

import (
	"errors"
	"fmt"

	"github.com/ta2gch/gazelle/core/class"
	"github.com/ta2gch/gazelle/core/class/cons"
	"github.com/ta2gch/gazelle/core/class/function"
	env "github.com/ta2gch/gazelle/core/environment"
)

func evalArguments(args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
	if args.Class() == class.Null {
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

func evalFunction(obj *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
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
	if car.Class() == class.Cons {
		caar, err := cons.Car(car)
		if err != nil {
			return nil, err
		}
		if *caar == *class.Symbol.New("lambda") {
			fun, err := Eval(car, local, global)
			if err != nil {
				return nil, err
			}
			ret, err := function.Apply(fun, cdr, local, global)
			if err != nil {
				return nil, err
			}
			return ret, nil
		}
	}
	if car.Class() != class.Symbol {
		return nil, fmt.Errorf("%v is not a symbol", obj.Value())
	}
	// get macro instance has value of Function interface
	var mac *class.Instance
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
	var fun *class.Instance
	if f, ok := local.GetFunction(car); ok {
		fun = f
	}
	if f, ok := global.GetFunction(car); ok {
		fun = f
	}
	if fun != nil {
		a, err := evalArguments(cdr, local, global)
		if err != nil {
			return nil, err
		}
		env := env.New()
		env.MergeDynamicVariable(local)
		r, err := function.Apply(fun, a, env, global)
		if err != nil {
			return nil, err
		}
		return r, nil
	}
	return nil, fmt.Errorf("%v is not defined", obj.Value())
}

// Eval evaluates any classs
func Eval(obj *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
	if obj.Class() == class.Null {
		return class.Null.New(nil), nil
	}
	switch obj.Class() {
	case class.Symbol:
		if val, ok := local.GetVariable(obj); ok {
			return val, nil
		}
		if val, ok := global.GetVariable(obj); ok {
			return val, nil
		}
		return nil, fmt.Errorf("%v is not defined", obj.Value())
	case class.Cons: // obj is a form or a macro
		ret, err := evalFunction(obj, local, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	case class.Integer, class.Float, class.Character, class.String:
		return obj, nil
	}
	return nil, errors.New("I have no ideas")
}
