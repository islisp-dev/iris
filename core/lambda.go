package core

import (
	"github.com/ta2gch/gazelle/core/class"
	"github.com/ta2gch/gazelle/core/class/cons"
	env "github.com/ta2gch/gazelle/core/environment"
)

type LambdaFunction struct {
	args  *class.Instance
	body  *class.Instance
	local *env.Environment
}

func NewLambdaFunction(args *class.Instance, body *class.Instance, local *env.Environment) *class.Instance {
	return class.Function.New(&LambdaFunction{args, body, local})
}

func (f LambdaFunction) Apply(args *class.Instance, local *env.Environment, global *env.Environment) (*class.Instance, error) {
	local.MergeAll(f.local)
	fargs := f.args
	aargs := args
	for fargs.Class() != class.Null {
		key, err := cons.Car(fargs)
		if err != nil {
			return nil, err
		}
		value, err := cons.Car(aargs)
		if err != nil {
			return nil, err
		}
		local.SetVariable(key, value)
		fargs, err = cons.Cdr(fargs)
		if err != nil {
			return nil, err
		}
		aargs, err = cons.Cdr(aargs)
		if err != nil {
			return nil, err
		}
	}
	body := f.body
	var ret *class.Instance
	for body.Class() != class.Null {
		exp, err := cons.Car(body)
		if err != nil {
			return nil, err
		}
		ret, err = Eval(exp, local, global)
		if err != nil {
			return nil, err
		}
		body, err = cons.Cdr(body)
		if err != nil {
			return nil, err
		}
	}
	return ret, nil
}
