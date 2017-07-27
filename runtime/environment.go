package runtime

import (
	"github.com/ta2gch/gazelle/runtime/object"
)

// Env struct is the struct for keeping functions and variables
type Env struct {
	Fun map[string]*object.Object
	Var map[string]*object.Object
}

// NewEnv creates new environment
func NewEnv() *Env {
	env := new(Env)
	env.Fun = map[string]*object.Object{}
	env.Var = map[string]*object.Object{}
	return env
}
