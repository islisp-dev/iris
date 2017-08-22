// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package environment

import (
	"github.com/ta2gch/iris/runtime/ilos"
)

// Environment struct is the struct for keeping functions and variables
type Environment struct {
	// Lexical
	BlockTag   stack
	TagbodyTag stack
	Function   stack
	Variable   stack

	// Global
	Class    stack
	Macro    stack
	Special  stack
	Property map2
	GensymID int
	Constant stack

	// Dynamic
	CatchTag        stack
	DynamicVariable stack // deep biding
	StandardInput   ilos.Instance
	StandardOutput  ilos.Instance
	ErrorOutput     ilos.Instance
	Handler         stack
}

// New creates new environment
func NewEnvironment(stdin, stdout, stderr ilos.Instance) Environment {
	env := new(Environment)

	// Lexical
	env.BlockTag = NewStack()
	env.TagbodyTag = NewStack()
	env.Function = NewStack()
	env.Variable = NewStack()

	// Global
	env.Macro = NewStack()
	env.Class = NewStack()
	env.Special = NewStack()
	env.Constant = NewStack()
	env.Property = NewMap2()
	env.GensymID = 0

	// Dynamic
	env.CatchTag = NewStack()
	env.DynamicVariable = NewStack()
	env.StandardInput = stdin
	env.StandardOutput = stdout
	env.ErrorOutput = stderr
	return *env
}

func (env *Environment) Merge(before Environment) {
	env.Class = append(before.Class, env.Class...)
	env.BlockTag = append(before.BlockTag, env.BlockTag...)
	env.TagbodyTag = append(before.TagbodyTag, env.TagbodyTag...)
	env.CatchTag = append(before.CatchTag, env.CatchTag...)
	env.Variable = append(before.Variable, env.Variable...)
	env.Function = append(before.Function, env.Function...)
	env.Special = append(before.Special, env.Special...)
	env.Macro = append(before.Macro, env.Macro...)
	env.DynamicVariable = append(before.DynamicVariable, env.DynamicVariable...)
	env.Constant = append(before.Constant, env.Constant...)
	env.Handler = append(before.Handler, env.Handler...)
	env.GensymID = before.GensymID
	env.Property = before.Property
	env.StandardInput = before.StandardInput
	env.StandardOutput = before.StandardOutput
	env.ErrorOutput = before.ErrorOutput
}

func (env *Environment) MergeDynamic(before Environment) {
	env.CatchTag = append(before.CatchTag, env.CatchTag...)
	env.DynamicVariable = append(before.DynamicVariable, env.DynamicVariable...)
	env.StandardInput = before.StandardInput
	env.StandardOutput = before.StandardOutput
	env.ErrorOutput = before.ErrorOutput
	env.Handler = append(before.Handler, env.Handler...)
}

func Push(before Environment) Environment {
	env := NewEnvironment(before.StandardInput, before.StandardOutput, before.ErrorOutput)
	env.Merge(before)
	return env
}

func PushDynamic(before Environment) Environment {
	env := NewEnvironment(before.StandardInput, before.StandardOutput, before.ErrorOutput)
	env.MergeDynamic(before)
	return env
}
