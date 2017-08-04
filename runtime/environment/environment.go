package environment

// Environment struct is the struct for keeping functions and variables
type Environment struct {
	ThrowTag        stack
	Macro           stack
	Function        stack
	Variable        stack
	DynamicVariable stack // deep biding
}

// New creates new environment
func New() *Environment {
	env := new(Environment)
	env.ThrowTag = newStack()
	env.Macro = newStack()
	env.Function = newStack()
	env.Variable = newStack()
	env.DynamicVariable = newStack()
	return env
}

// TopLevel is a global environment
var TopLevel = New()
