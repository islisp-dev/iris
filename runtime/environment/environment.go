// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package environment

// Environment struct is the struct for keeping functions and variables
type Environment struct {
	BlockTag        stack
	TagbodyTag      stack
	CatchTag        stack
	Macro           stack
	Function        stack
	Special         stack
	Variable        stack
	DynamicVariable stack // deep biding
	Constant        stack
}

// New creates new environment
func New() *Environment {
	env := new(Environment)
	env.BlockTag = newStack()
	env.TagbodyTag = newStack()
	env.CatchTag = newStack()
	env.Macro = newStack()
	env.Function = newStack()
	env.Special = newStack()
	env.Variable = newStack()
	env.DynamicVariable = newStack()
	env.Constant = newStack()
	return env
}

func (env *Environment) Merge(before *Environment) *Environment {
	env.BlockTag = append(before.BlockTag, env.BlockTag...)
	env.TagbodyTag = append(before.TagbodyTag, env.TagbodyTag...)
	env.CatchTag = append(before.CatchTag, env.CatchTag...)
	env.Variable = append(before.Variable, env.Variable...)
	env.Function = append(before.Function, env.Function...)
	env.Special = append(before.Special, env.Special...)
	env.Macro = append(before.Macro, env.Macro...)
	env.DynamicVariable = append(before.DynamicVariable, env.DynamicVariable...)
	env.Constant = append(before.Constant, env.Constant...)
	return env
}

// TopLevel is a global environment
var TopLevel = New()
