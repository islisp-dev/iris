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
	if car.Class() != class.Symbol {
		return nil, fmt.Errorf("%v is not a symbol", obj.Value())
	}
	// get function arguments
	args, err := cons.Cdr(obj)
	if err != nil {
		return nil, err
	}
	// get macro instance has value of Function interface
	var mac *class.Instance
	if m, ok := local.Macro[car.Value()]; ok {
		mac = m
	}
	if m, ok := global.Macro[car.Value()]; ok {
		mac = m
	}
	if mac != nil {
		ret, err := function.Apply(mac, args, local, global)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	// get function instance has value of Function interface
	var fun *class.Instance
	if f, ok := local.Function[car.Value()]; ok {
		fun = f
	}
	if f, ok := global.Function[car.Value()]; ok {
		fun = f
	}
	if fun != nil {
		a, err := evalArguments(args, local, global)
		if err != nil {
			return nil, err
		}
		env := env.New()
		env.DynamicVariable = append(local.DynamicVariable, env.DynamicVariable...)
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
		if val, ok := local.Variable[obj.Value()]; ok {
			return val, nil
		}
		if val, ok := global.Variable[obj.Value()]; ok {
			return val, nil
		}
		return nil, fmt.Errorf("%v is not defined", obj.Value())
	case class.List: // obj is a form or a macro
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
