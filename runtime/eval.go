package runtime

import (
	"errors"
	"fmt"

	"github.com/ta2gch/gazelle/runtime/class"
	"github.com/ta2gch/gazelle/runtime/object"
)

// Eval evaluates any objects
func Eval(obj *object.Object, local *Env, global *Env) (*object.Object, error) {
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
	case class.Integer, class.Float, class.Character, class.String:
		return obj, nil
	}
	return nil, errors.New("I have no ideas")
}
