// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

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
	e.BlockTag = before.BlockTag.Append(e.BlockTag[1:])
	e.TagbodyTag = before.TagbodyTag.Append(e.TagbodyTag[1:])
	e.Variable = before.Variable.Append(e.Variable[1:])
	e.Function = before.Function.Append(e.Function[1:])

	e.Macro = before.Macro.Append(e.Macro[1:])
	e.Class = before.Class.Append(e.Class[1:])
	e.Special = before.Special.Append(e.Special[1:])
	e.Constant = before.Constant.Append(e.Constant[1:])
	e.Property = before.Property

	e.CatchTag = before.CatchTag.Append(e.CatchTag[1:])
	e.DynamicVariable = before.DynamicVariable.Append(e.DynamicVariable[1:])
	e.StandardInput = before.StandardInput
	e.StandardOutput = before.StandardOutput
	e.ErrorOutput = before.ErrorOutput
	e.Handler = before.Handler
}

func (before *Environment) NewLexical() Environment {
	e := NewEnvironment(before.StandardInput, before.StandardOutput, before.ErrorOutput, before.Handler)

	e.BlockTag = before.BlockTag.Append(e.BlockTag)
	e.TagbodyTag = before.TagbodyTag.Append(e.TagbodyTag)
	e.Variable = before.Variable.Append(e.Variable)
	e.Function = before.Function.Append(e.Function)

	e.Macro = before.Macro.Append(e.Macro)
	e.Class = before.Class.Append(e.Class)
	e.Special = before.Special.Append(e.Special)
	e.Constant = before.Constant.Append(e.Constant)
	e.Property = before.Property

	e.CatchTag = before.CatchTag.Append(e.CatchTag)
	e.DynamicVariable = before.DynamicVariable.Append(e.DynamicVariable)
	e.StandardInput = before.StandardInput
	e.StandardOutput = before.StandardOutput
	e.ErrorOutput = before.ErrorOutput
	e.Handler = before.Handler

	return e
}

func (before *Environment) NewDynamic() Environment {
	e := NewEnvironment(before.StandardInput, before.StandardOutput, before.ErrorOutput, before.Handler)

	e.BlockTag = stack{before.BlockTag[0]}.Append(e.BlockTag)
	e.TagbodyTag = stack{before.TagbodyTag[0]}.Append(e.TagbodyTag)
	e.Variable = stack{before.Variable[0]}.Append(e.Variable)
	e.Function = stack{before.Function[0]}.Append(e.Function)

	e.Macro = stack{before.Macro[0]}.Append(e.Macro)
	e.Class = stack{before.Class[0]}.Append(e.Class)
	e.Special = stack{before.Special[0]}.Append(e.Special)
	e.Constant = stack{before.Constant[0]}.Append(e.Constant)
	e.Property = before.Property

	e.CatchTag = before.CatchTag.Append(e.CatchTag)
	e.DynamicVariable = before.DynamicVariable.Append(e.DynamicVariable)
	e.StandardInput = before.StandardInput
	e.StandardOutput = before.StandardOutput
	e.ErrorOutput = before.ErrorOutput
	e.Handler = before.Handler

	return e
}
