// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package env

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
	Handler         ilos.Instance
}

// New creates new eironment
func NewEnvironment(stdin, stdout, stderr, handler ilos.Instance) Environment {
	e := new(Environment)

	// Lexical
	e.BlockTag = NewStack()
	e.TagbodyTag = NewStack()
	e.Function = NewStack()
	e.Variable = NewStack()

	// Global
	e.Macro = NewStack()
	e.Class = NewStack()
	e.Special = NewStack()
	e.Constant = NewStack()
	e.Property = NewMap2()
	e.GensymID = 0

	// Dynamic
	e.CatchTag = NewStack()
	e.DynamicVariable = NewStack()
	e.StandardInput = stdin
	e.StandardOutput = stdout
	e.ErrorOutput = stderr
	e.Handler = handler
	return *e
}

func (e *Environment) MergeLexical(before Environment) {
	e.BlockTag = append(before.BlockTag, e.BlockTag[1:]...)
	e.TagbodyTag = append(before.TagbodyTag, e.TagbodyTag[1:]...)
	e.Variable = append(before.Variable, e.Variable[1:]...)
	e.Function = append(before.Function, e.Function[1:]...)

	e.Macro = append(before.Macro, e.Macro[1:]...)
	e.Class = append(before.Class, e.Class[1:]...)
	e.Special = append(before.Special, e.Special[1:]...)
	e.Constant = append(before.Constant, e.Constant[1:]...)
	e.Property = before.Property
	e.GensymID = before.GensymID

	e.CatchTag = append(before.CatchTag, e.CatchTag[1:]...)
	e.DynamicVariable = append(before.DynamicVariable, e.DynamicVariable[1:]...)
	e.StandardInput = before.StandardInput
	e.StandardOutput = before.StandardOutput
	e.ErrorOutput = before.ErrorOutput
	e.Handler = before.Handler
}

func (e *Environment) MergeDynamic(before Environment) {
	e.BlockTag = append(stack{before.BlockTag[0]}, e.BlockTag[1:]...)
	e.TagbodyTag = append(stack{before.TagbodyTag[0]}, e.TagbodyTag[1:]...)
	e.Variable = append(stack{before.Variable[0]}, e.Variable[1:]...)
	e.Function = append(stack{before.Function[0]}, e.Function[1:]...)

	e.Macro = append(stack{before.Macro[0]}, e.Macro[1:]...)
	e.Class = append(stack{before.Class[0]}, e.Class[1:]...)
	e.Special = append(stack{before.Special[0]}, e.Special[1:]...)
	e.Constant = append(stack{before.Constant[0]}, e.Constant[1:]...)
	e.Property = before.Property
	e.GensymID = before.GensymID

	e.CatchTag = append(before.CatchTag, e.CatchTag[1:]...)
	e.DynamicVariable = append(before.DynamicVariable, e.DynamicVariable[1:]...)
	e.StandardInput = before.StandardInput
	e.StandardOutput = before.StandardOutput
	e.ErrorOutput = before.ErrorOutput
	e.Handler = before.Handler
}

func (before *Environment) NewLexical() Environment {
	e := NewEnvironment(before.StandardInput, before.StandardOutput, before.ErrorOutput, before.Handler)

	e.BlockTag = append(before.BlockTag, e.BlockTag[0])
	e.TagbodyTag = append(before.TagbodyTag, e.TagbodyTag[0])
	e.Variable = append(before.Variable, e.Variable[0])
	e.Function = append(before.Function, e.Function[0])

	e.Macro = append(before.Macro, e.Macro[0])
	e.Class = append(before.Class, e.Class[0])
	e.Special = append(before.Special, e.Special[0])
	e.Constant = append(before.Constant, e.Constant[0])
	e.Property = before.Property
	e.GensymID = before.GensymID

	e.CatchTag = append(before.CatchTag, e.CatchTag[0])
	e.DynamicVariable = append(before.DynamicVariable, e.DynamicVariable[0])
	e.StandardInput = before.StandardInput
	e.StandardOutput = before.StandardOutput
	e.ErrorOutput = before.ErrorOutput
	e.Handler = before.Handler

	return e
}

func (before *Environment) NewDynamic() Environment {
	e := NewEnvironment(before.StandardInput, before.StandardOutput, before.ErrorOutput, before.Handler)

	e.BlockTag = append(stack{before.BlockTag[0]}, e.BlockTag[0])
	e.TagbodyTag = append(stack{before.TagbodyTag[0]}, e.TagbodyTag[0])
	e.Variable = append(stack{before.Variable[0]}, e.Variable[0])
	e.Function = append(stack{before.Function[0]}, e.Function[0])

	e.Macro = append(stack{before.Macro[0]}, e.Macro[0])
	e.Class = append(stack{before.Class[0]}, e.Class[0])
	e.Special = append(stack{before.Special[0]}, e.Special[0])
	e.Constant = append(stack{before.Constant[0]}, e.Constant[0])
	e.Property = before.Property
	e.GensymID = before.GensymID

	e.CatchTag = append(before.CatchTag, e.CatchTag[0])
	e.DynamicVariable = append(before.DynamicVariable, e.DynamicVariable[0])
	e.StandardInput = before.StandardInput
	e.StandardOutput = before.StandardOutput
	e.ErrorOutput = before.ErrorOutput
	e.Handler = before.Handler

	return e
}
