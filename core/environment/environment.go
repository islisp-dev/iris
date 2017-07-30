package environment

import (
	"github.com/ta2gch/gazelle/core/class"
)

// Environment struct is the struct for keeping functions and variables
type Environment struct {
	Macro           []map[class.Value]class.Instance
	Function        []map[class.Value]class.Instance
	Variable        []map[class.Value]class.Instance
	DynamicVariable []map[class.Value]class.Instance // deep biding
}

// New creates new environment
func New() *Environment {
	env := new(Environment)
	env.Macro = []map[class.Value]class.Instance{map[class.Value]class.Instance{}}
	env.Function = []map[class.Value]class.Instance{map[class.Value]class.Instance{}}
	env.Variable = []map[class.Value]class.Instance{map[class.Value]class.Instance{}}
	env.DynamicVariable = []map[class.Value]class.Instance{map[class.Value]class.Instance{}}
	return env
}

func (e *Environment) MergeDynamicVariable(f *Environment) {
	e.DynamicVariable = append(e.DynamicVariable, f.DynamicVariable...)
}

func (e *Environment) MergeAll(f *Environment) {
	e.Variable = append(e.Variable, f.Variable...)
	e.Function = append(e.Function, f.Function...)
	e.Macro = append(e.Macro, f.Macro...)
	e.DynamicVariable = append(e.DynamicVariable, f.DynamicVariable...)
}

func (e *Environment) GetVariable(key class.Instance) (class.Instance, bool) {
	for _, vars := range e.Variable {
		if v, ok := vars[key.Value()]; ok {
			return v, ok
		}
	}
	return nil, false
}

func (e *Environment) SetVariable(key class.Instance, value class.Instance) {
	for _, vars := range e.Variable {
		if _, ok := vars[key.Value()]; ok {
			vars[key.Value()] = value
			return
		}
	}
	e.Variable[0][key.Value()] = value
}

func (e *Environment) GetFunction(key class.Instance) (class.Instance, bool) {
	for _, vars := range e.Function {
		if v, ok := vars[key.Value()]; ok {
			return v, ok
		}
	}
	return nil, false
}

func (e *Environment) SetFunction(key class.Instance, value class.Instance) {
	for _, vars := range e.Function {
		if _, ok := vars[key.Value()]; ok {
			vars[key.Value()] = value
			return
		}
	}
	e.Function[0][key.Value()] = value
}

func (e *Environment) GetMacro(key class.Instance) (class.Instance, bool) {
	for _, vars := range e.Macro {
		if v, ok := vars[key.Value()]; ok {
			return v, ok
		}
	}
	return nil, false
}

func (e *Environment) SetMacro(key class.Instance, value class.Instance) {
	for _, vars := range e.Macro {
		if _, ok := vars[key.Value()]; ok {
			vars[key.Value()] = value
			return
		}
	}
	e.Macro[0][key.Value()] = value
}

func (e *Environment) GetDynamicVariable(key class.Instance) (class.Instance, bool) {
	for _, vars := range e.DynamicVariable {
		if v, ok := vars[key.Value()]; ok {
			return v, ok
		}
	}
	return nil, false
}

func (e *Environment) SetDynamicVariable(key class.Instance, value class.Instance) {
	for _, vars := range e.DynamicVariable {
		if _, ok := vars[key.Value()]; ok {
			vars[key.Value()] = value
			return
		}
	}
	e.DynamicVariable[0][key.Value()] = value
}

// TopLevel is a global environment
var TopLevel = New()
