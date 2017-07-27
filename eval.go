package main

import (
	"errors"
	"fmt"
)

// Env struct is the struct for keeping functions and variables
type Env struct {
	Frm map[string]Function
	Mac map[string]Function
	Fun map[string]Function
	Var map[string]*Object
}

// NewEnv creates new environment
func NewEnv() *Env {
	env := new(Env)
	env.Frm = map[string]Function{}
	env.Mac = map[string]Function{}
	env.Fun = map[string]Function{}
	env.Var = map[string]*Object{}
	return env
}

// Eval evaluates any objects
func Eval(obj *Object, local *Env, global *Env) (*Object, error) {
	if obj == nil {
		return nil, nil
	}
	switch obj.Type {
	case "symbol":
		if val, ok := local.Var[obj.Val.(string)]; ok {
			return val, nil
		}
		if val, ok := global.Var[obj.Val.(string)]; ok {
			return val, nil
		}
		return nil, fmt.Errorf("%v is not defined", obj.Val)
	case "list":
		if obj.Car.Type != "symbol" {
			return nil, fmt.Errorf("%v is not a function", obj.Car.Val)
		}
		args := obj.Cdr
		/*
			if frm, ok := global.Frm[obj.Car.Val.(string)]; ok {
				ret, err := frm.Apply(args, local, global)
				if err != nil {
					return nil, err
				}
				ret, err = Eval(ret, local, global)
				if err != nil {
					return nil, err
				}
				return ret, nil
			}
			if mac, ok := global.Mac[obj.Car.Val.(string)]; ok {
				ret, err := mac.Apply(args, NewEnv(), global)
				if err != nil {
					return nil, err
				}
				ret, err = Eval(ret, local, global)
				if err != nil {
					return nil, err
				}
				return ret, nil
			}
		*/
		cdr := NewCons(nil, nil)
		tmp := cdr
		for args != nil {
			car, err := Eval(args.Car, local, global)
			if err != nil {
				return nil, err
			}
			cdr.Car = car
			cdr = cdr.Cdr
			args = args.Cdr
		}
		args = tmp
		if fun, ok := local.Fun[obj.Car.Val.(string)]; ok {
			ret, err := fun.Apply(args, NewEnv(), global)
			if err != nil {
				return nil, err
			}
			return ret, nil
		}
		if fun, ok := global.Fun[obj.Car.Val.(string)]; ok {
			ret, err := fun.Apply(args, NewEnv(), global)
			if err != nil {
				return nil, err
			}
			return ret, nil
		}
		return nil, fmt.Errorf("%v is not defined", obj.Val)
	case "integer", "float", "character", "string":
		return obj, nil
	}
	return nil, errors.New("I have no ideas")
}
