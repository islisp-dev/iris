package core

import (
	"github.com/ta2gch/gazelle/core/class"
	"github.com/ta2gch/gazelle/core/class/cons"
	env "github.com/ta2gch/gazelle/core/environment"
)

type LambdaFunction struct {
	lambdaList class.Instance
	forms      class.Instance
	local      *env.Environment
}

func NewLambdaFunction(lambdaList class.Instance, forms class.Instance, local *env.Environment) class.Instance {
	return class.New(class.Function, LambdaFunction{lambdaList, forms, local})
}

func (f LambdaFunction) Apply(args class.Instance, local *env.Environment, global *env.Environment) (class.Instance, class.Instance) {
	local.MergeAll(f.local)
	fargs := f.lambdaList
	aargs := args
	for !fargs.IsInstanceOf(class.Null) {
		key, err := cons.Car(fargs)
		if err != nil {
			return nil, err
		}
		value, err := cons.Car(aargs)
		if err != nil {
			return nil, err
		}
		if key == class.New(class.Symbol, ":rest") || key == class.New(class.Symbol, "&rest") {
			cdr, err := cons.Cdr(fargs)
			if err != nil {
				return nil, err
			}
			cadr, err := cons.Car(cdr)
			if err != nil {
				return nil, err
			}
			cddr, err := cons.Cdr(cdr)
			if err != nil {
				return nil, err
			}
			if !cddr.IsInstanceOf(class.Null) {
				return nil, class.New(class.ParseError, nil)
			}
			local.SetVariable(cadr, aargs)
			break
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
	body := f.forms
	var ret class.Instance
	for !body.IsInstanceOf(class.Null) {
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
