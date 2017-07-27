package core

import (
	"errors"
	"fmt"

	"github.com/ta2gch/gazelle/core/class"
)

// Eval evaluates any classs
func Eval(obj *class.Instance, local *Env, global *Env) (*class.Instance, error) {
	if obj == nil {
		return nil, nil
	}
	switch obj.Class {
	case class.Symbol:
		if val, ok := local.Var[obj.Val.(string)]; ok {
			return val, nil
		}
		if val, ok := global.Var[obj.Val.(string)]; ok {
			return val, nil
		}
		return nil, fmt.Errorf("%v is not defined", obj.Val)
	case class.List:
		if obj.Val.(class.Cell).Car.Class != class.Symbol {
			return nil, fmt.Errorf("%v is not a symbol", obj.Val)
		}
		if f, ok := local.Fun[obj.Val.(class.Cell).Car.Val.(string)]; ok {
			// TODO: Evaluate each arguments
			r, err := f.Val.(Function).Apply(obj.Val.(class.Cell).Cdr, global)
			return r, err
		}
		if f, ok := global.Fun[obj.Val.(class.Cell).Car.Val.(string)]; ok {
			// TODO: Evaluate each arguments
			r, err := f.Val.(Function).Apply(obj.Val.(class.Cell).Cdr, global)
			return r, err
		}
	case class.Integer, class.Float, class.Character, class.String:
		return obj, nil
	}
	return nil, errors.New("I have no ideas")
}
