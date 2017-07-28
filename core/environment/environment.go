package environment

import (
	"github.com/ta2gch/gazelle/core/class"
)

// Environment struct is the struct for keeping functions and variables
type Environment struct {
	Function map[string]*class.Instance
	Variable map[string]*class.Instance
}

// New creates new environment
func New() *Environment {
	env := new(Environment)
	env.Function = map[string]*class.Instance{}
	env.Variable = map[string]*class.Instance{}
	return env
}

// TopLevel is a global environment
var TopLevel = New()
