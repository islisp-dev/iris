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
	env.Handler = NewStack()
	return *env
}

func (env *Environment) Merge(before Environment) {
	env.BlockTag = append(before.BlockTag, env.BlockTag[1:]...)
	env.TagbodyTag = append(before.TagbodyTag, env.TagbodyTag[1:]...)
	env.Variable = append(before.Variable, env.Variable[1:]...)
	env.Function = append(before.Function, env.Function[1:]...)

	env.Macro = append(before.Macro, env.Macro[1:]...)
	env.Class = append(before.Class, env.Class[1:]...)
	env.Special = append(before.Special, env.Special[1:]...)
	env.Constant = append(before.Constant, env.Constant[1:]...)
	env.Property = before.Property
	env.GensymID = before.GensymID

	env.CatchTag = append(before.CatchTag, env.CatchTag[1:]...)
	env.DynamicVariable = append(before.DynamicVariable, env.DynamicVariable[1:]...)
	env.StandardInput = before.StandardInput
	env.StandardOutput = before.StandardOutput
	env.ErrorOutput = before.ErrorOutput
	env.Handler = append(before.Handler, env.Handler[1:]...)
}

func (env *Environment) MergeDynamic(before Environment) {
	env.BlockTag = append(stack{before.BlockTag[0]}, env.BlockTag[1:]...)
	env.TagbodyTag = append(stack{before.TagbodyTag[0]}, env.TagbodyTag[1:]...)
	env.Variable = append(stack{before.Variable[0]}, env.Variable[1:]...)
	env.Function = append(stack{before.Function[0]}, env.Function[1:]...)

	env.Macro = append(stack{before.Macro[0]}, env.Macro[1:]...)
	env.Class = append(stack{before.Class[0]}, env.Class[1:]...)
	env.Special = append(stack{before.Special[0]}, env.Special[1:]...)
	env.Constant = append(stack{before.Constant[0]}, env.Constant[1:]...)
	env.Property = before.Property
	env.GensymID = before.GensymID

	env.CatchTag = append(before.CatchTag, env.CatchTag[1:]...)
	env.DynamicVariable = append(before.DynamicVariable, env.DynamicVariable[1:]...)
	env.StandardInput = before.StandardInput
	env.StandardOutput = before.StandardOutput
	env.ErrorOutput = before.ErrorOutput
	env.Handler = append(before.Handler, env.Handler[1:]...)
}

func Push(before Environment) Environment {
	env := NewEnvironment(before.StandardInput, before.StandardOutput, before.ErrorOutput)

	env.BlockTag = append(before.BlockTag, env.BlockTag[0])
	env.TagbodyTag = append(before.TagbodyTag, env.TagbodyTag[0])
	env.Variable = append(before.Variable, env.Variable[0])
	env.Function = append(before.Function, env.Function[0])

	env.Macro = append(before.Macro, env.Macro[0])
	env.Class = append(before.Class, env.Class[0])
	env.Special = append(before.Special, env.Special[0])
	env.Constant = append(before.Constant, env.Constant[0])
	env.Property = before.Property
	env.GensymID = before.GensymID

	env.CatchTag = append(before.CatchTag, env.CatchTag[0])
	env.DynamicVariable = append(before.DynamicVariable, env.DynamicVariable[0])
	env.StandardInput = before.StandardInput
	env.StandardOutput = before.StandardOutput
	env.ErrorOutput = before.ErrorOutput
	env.Handler = append(before.Handler, env.Handler[0])

	return env
}

func PushDynamic(before Environment) Environment {
	env := NewEnvironment(before.StandardInput, before.StandardOutput, before.ErrorOutput)

	env.BlockTag = append(stack{before.BlockTag[0]}, env.BlockTag[0])
	env.TagbodyTag = append(stack{before.TagbodyTag[0]}, env.TagbodyTag[0])
	env.Variable = append(stack{before.Variable[0]}, env.Variable[0])
	env.Function = append(stack{before.Function[0]}, env.Function[0])

	env.Macro = append(stack{before.Macro[0]}, env.Macro[0])
	env.Class = append(stack{before.Class[0]}, env.Class[0])
	env.Special = append(stack{before.Special[0]}, env.Special[0])
	env.Constant = append(stack{before.Constant[0]}, env.Constant[0])
	env.Property = before.Property
	env.GensymID = before.GensymID

	env.CatchTag = append(before.CatchTag, env.CatchTag[0])
	env.DynamicVariable = append(before.DynamicVariable, env.DynamicVariable[0])
	env.StandardInput = before.StandardInput
	env.StandardOutput = before.StandardOutput
	env.ErrorOutput = before.ErrorOutput
	env.Handler = append(before.Handler, env.Handler[0])

	return env
}
