package main

import (
	"errors"
	"fmt"
)

type Env struct {
	Fun map[string]func(*Object, *Env, *Env) (*Object, error)
	Var map[string]*Object
}

func NewEnv() *Env {
	env := new(Env)
	env.Fun = map[string]func(*Object, *Env, *Env) (*Object, error){}
	env.Var = map[string]*Object{}
	return env
}

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
		return nil, fmt.Errorf("%s is not defined", obj.Val)
	case "list":
		// funcall
	case "integer", "float", "character", "string":
		return obj, nil
	}
	return nil, errors.New("I have no ideas")
}
