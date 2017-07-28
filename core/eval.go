package core

import (
	"errors"
	"fmt"

	"github.com/ta2gch/gazelle/core/class"
	"github.com/ta2gch/gazelle/core/class/cons"
	"github.com/ta2gch/gazelle/core/class/function"
	"github.com/ta2gch/gazelle/core/environment"
)

func evalArgs(args *class.Instance, local *environment.Environment, global *environment.Environment) (*class.Instance, error) {
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
	b, err := evalArgs(cdr, local, global)
	if err != nil {
		return nil, err
	}
	return cons.New(a, b), nil
}

// Eval evaluates any classs
func Eval(obj *class.Instance, local *environment.Environment, global *environment.Environment) (*class.Instance, error) {
	if obj.Class() == class.Null {
		return class.Null.New(nil), nil
	}
	switch obj.Class() {
	case class.Symbol:
		if val, ok := local.Variable[obj.Value().(string)]; ok {
			return val, nil
		}
		if val, ok := global.Variable[obj.Value().(string)]; ok {
			return val, nil
		}
		return nil, fmt.Errorf("%v is not defined", obj.Value())
	case class.List:
		car, err := cons.Car(obj)
		if err != nil {
			return nil, err
		}
		cdr, err := cons.Cdr(obj)
		if err != nil {
			return nil, err
		}
		if car.Class() != class.Symbol {
			return nil, fmt.Errorf("%v is not a symbol", obj.Value())
		}
		if f, ok := local.Function[car.Value().(string)]; ok {
			a, err := evalArgs(cdr, local, global)
			if err != nil {
				return nil, err
			}
			r, err := function.Apply(f, a, global)
			if err != nil {
				return nil, err
			}
			return r, nil
		}
		if f, ok := global.Function[car.Value().(string)]; ok {
			a, err := evalArgs(cdr, local, global)
			if err != nil {
				return nil, err
			}
			r, err := function.Apply(f, a, global)
			if err != nil {
				return nil, err
			}
			return r, nil
		}
	case class.Integer, class.Float, class.Character, class.String:
		return obj, nil
	}
	return nil, errors.New("I have no ideas")
}
