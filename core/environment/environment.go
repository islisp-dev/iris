package environment

import (
	"github.com/ta2gch/gazelle/core/class"
)

// Env struct is the struct for keeping functions and variables
type Env struct {
	Fun map[string]*class.Instance
	Var map[string]*class.Instance
}

// New creates new environment
func New() *Env {
	env := new(Env)
	env.Fun = map[string]*class.Instance{}
	env.Var = map[string]*class.Instance{}
	return env
}

// TopLevel is a global environment
var TopLevel = New()
