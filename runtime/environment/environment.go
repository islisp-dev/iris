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
	return env
}

// TopLevel is a global environment
var TopLevel = New()
