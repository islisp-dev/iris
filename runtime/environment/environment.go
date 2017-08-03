package environment

import "github.com/ta2gch/iris/runtime/ilos"

// Environment struct is the struct for keeping functions and variables
type Environment struct {
	Macro           []map[ilos.Instance]ilos.Instance
	Function        []map[ilos.Instance]ilos.Instance
	Variable        []map[ilos.Instance]ilos.Instance
	DynamicVariable []map[ilos.Instance]ilos.Instance // deep biding
}

// New creates new environment
func New() *Environment {
	env := new(Environment)
	env.Macro = []map[ilos.Instance]ilos.Instance{map[ilos.Instance]ilos.Instance{}}
	env.Function = []map[ilos.Instance]ilos.Instance{map[ilos.Instance]ilos.Instance{}}
	env.Variable = []map[ilos.Instance]ilos.Instance{map[ilos.Instance]ilos.Instance{}}
	env.DynamicVariable = []map[ilos.Instance]ilos.Instance{map[ilos.Instance]ilos.Instance{}}
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

func (e *Environment) GetVariable(key ilos.Instance) (ilos.Instance, bool) {
	for _, vars := range e.Variable {
		if v, ok := vars[key]; ok {
			return v, ok
		}
	}
	return nil, false
}

func (e *Environment) SetVariable(key ilos.Instance, value ilos.Instance) {
	for _, vars := range e.Variable {
		if _, ok := vars[key]; ok {
			vars[key] = value
			return
		}
	}
	e.Variable[0][key] = value
}

func (e *Environment) GetFunction(key ilos.Instance) (ilos.Instance, bool) {
	for _, vars := range e.Function {
		if v, ok := vars[key]; ok {
			return v, ok
		}
	}
	return nil, false
}

func (e *Environment) SetFunction(key ilos.Instance, value ilos.Instance) {
	for _, vars := range e.Function {
		if _, ok := vars[key]; ok {
			vars[key] = value
			return
		}
	}
	e.Function[0][key] = value
}

func (e *Environment) GetMacro(key ilos.Instance) (ilos.Instance, bool) {
	for _, vars := range e.Macro {
		if v, ok := vars[key]; ok {
			return v, ok
		}
	}
	return nil, false
}

func (e *Environment) SetMacro(key ilos.Instance, value ilos.Instance) {
	for _, vars := range e.Macro {
		if _, ok := vars[key]; ok {
			vars[key] = value
			return
		}
	}
	e.Macro[0][key] = value
}

func (e *Environment) GetDynamicVariable(key ilos.Instance) (ilos.Instance, bool) {
	for _, vars := range e.DynamicVariable {
		if v, ok := vars[key]; ok {
			return v, ok
		}
	}
	return nil, false
}

func (e *Environment) SetDynamicVariable(key ilos.Instance, value ilos.Instance) {
	for _, vars := range e.DynamicVariable {
		if _, ok := vars[key]; ok {
			vars[key] = value
			return
		}
	}
	e.DynamicVariable[0][key] = value
}

// TopLevel is a global environment
var TopLevel = New()
