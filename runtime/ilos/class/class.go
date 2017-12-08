// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package class

import (
	"github.com/asciian/iris/runtime/ilos/instance"
)

var Object = instance.ObjectClass
var BuiltInClass = instance.BuiltInClassClass
var StandardClass = instance.StandardClassClass
var BasicArray = instance.BasicArrayClass
var BasicArrayStar = instance.BasicArrayStarClass
var GeneralArrayStar = instance.GeneralArrayStarClass
var BasicVector = instance.BasicVectorClass
var GeneralVector = instance.GeneralVectorClass
var String = instance.StringClass
var Character = instance.CharacterClass
var Function = instance.FunctionClass
var GenericFunction = instance.GenericFunctionClass
var StandardGenericFunction = instance.StandardGenericFunctionClass
var List = instance.ListClass
var Cons = instance.ConsClass
var Null = instance.NullClass
var Symbol = instance.SymbolClass
var Number = instance.NumberClass
var Integer = instance.IntegerClass
var Float = instance.FloatClass

var SeriousCondition = instance.SeriousConditionClass
var Error = instance.ErrorClass
var ArithmeticError = instance.ArithmeticErrorClass
var DivisionByZero = instance.DivisionByZeroClass
var FloatingPointOnderflow = instance.FloatingPointOnderflowClass
var FloatingPointUnderflow = instance.FloatingPointUnderflowClass
var ControlError = instance.ControlErrorClass
var ParseError = instance.ParseErrorClass
var ProgramError = instance.ProgramErrorClass
var DomainError = instance.DomainErrorClass
var UndefinedEntity = instance.UndefinedEntityClass
var UndefinedVariable = instance.UndefinedVariableClass
var UndefinedFunction = instance.UndefinedFunctionClass
var SimpleError = instance.SimpleErrorClass
var StreamError = instance.StreamErrorClass
var EndOfStream = instance.EndOfStreamClass
var StorageExhausted = instance.StorageExhaustedClass
var StandardObject = instance.StandardObjectClass
var Stream = instance.StreamClass

// Implementation defined
var Escape = instance.EscapeClass
var CatchTag = instance.CatchTagClass
var TagbodyTag = instance.TagbodyTagClass
var BlockTag = instance.BlockTagClass
var Continue = instance.ContinueClass
