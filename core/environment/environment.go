package environment

import (
	"github.com/ta2gch/gazelle/core/class"
)

// Environment struct is the struct for keeping functions and variables
type Environment struct {
	Macro           map[interface{}]*class.Instance
	Function        map[interface{}]*class.Instance
	Variable        map[interface{}]*class.Instance
	DynamicVariable map[interface{}]*class.Instance
}

// New creates new environment
func New() *Environment {
	env := new(Environment)
	env.Macro = map[interface{}]*class.Instance{}
	env.Function = map[interface{}]*class.Instance{}
	env.Variable = map[interface{}]*class.Instance{}
	env.DynamicVariable = map[interface{}]*class.Instance{}
	return env
}

// TopLevel is a global environment
var TopLevel = New()
