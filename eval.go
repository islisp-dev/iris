package main

import (
	"errors"
	"fmt"
)

type Env struct {
	Fun map[string]func(*Object) (*Object, error)
	Var map[string]*Object
}

func NewEnv() *Env {
	env := new(Env)
	env.Fun = map[string]func(*Object) (*Object, error){}
	env.Var = map[string]*Object{}
	return env
}

func Eval(obj *Object, local *Env, global *Env) (*Object, error) {
	if obj.Type == "symbol" {
		if val, ok := local.Var[obj.Val.(string)]; ok {
			return val, nil
		}
		if val, ok := global.Var[obj.Val.(string)]; ok {
			return val, nil
		}
		return nil, fmt.Errorf("%s is not defined", obj.Val)
	}
	return nil, errors.New("I have no ideas")
}
