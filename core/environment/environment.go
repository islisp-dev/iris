package environment

import (
	"github.com/ta2gch/gazelle/core/class"
)

// Environment struct is the struct for keeping functions and variables
type Environment struct {
	Macro           map[class.Value]*class.Instance
	Function        map[class.Value]*class.Instance
	Variable        map[class.Value]*class.Instance
	DynamicVariable []map[class.Value]*class.Instance // deep biding
}

// New creates new environment
func New() *Environment {
	env := new(Environment)
	env.Macro = map[class.Value]*class.Instance{}
	env.Function = map[class.Value]*class.Instance{}
	env.Variable = map[class.Value]*class.Instance{}
	env.DynamicVariable = []map[class.Value]*class.Instance{map[class.Value]*class.Instance{}}
	return env
}

// TopLevel is a global environment
var TopLevel = New()
