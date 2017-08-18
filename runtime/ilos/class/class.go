// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package class

import "github.com/ta2gch/iris/runtime/ilos"
import "reflect"

func Is(c, p ilos.Class) bool {
	var sub func(c, p ilos.Class) bool
	sub = func(c, p ilos.Class) bool {
		if reflect.DeepEqual(c, p) {
			return true
		}
		for _, d := range c.Supers() {
			if sub(d, p) {
				return true
			}
		}
		return false
	}
	for _, d := range c.Supers() {
		if sub(d, p) {
			return true
		}
	}
	return false
}

var Object = builtinclass{[]ilos.Class{}, []string{}, "<OBJECT>"}
var BuiltInClass = builtinclass{[]ilos.Class{Object}, []string{}, "<BUILT-IN-CLASS>"}
var StandardClass = builtinclass{[]ilos.Class{Object}, []string{}, "<STANDARD-CLASS>"}
var BasicArray = builtinclass{[]ilos.Class{Object}, []string{}, "<BASIC-ARRAY>"}
var BasicArrayStar = builtinclass{[]ilos.Class{BasicArray}, []string{}, "<BASIC-ARRAY*>"}
var GeneralArrayStar = builtinclass{[]ilos.Class{BasicArrayStar}, []string{}, "<GENERAL-ARRAY*>"}
var BasicVector = builtinclass{[]ilos.Class{BasicArray}, []string{}, "<BASIC-VECTOR>"}
var GeneralVector = builtinclass{[]ilos.Class{BasicVector}, []string{}, "<GENERAL-VECTOR>"}
var String = builtinclass{[]ilos.Class{BasicVector}, []string{}, "<STRING>"}
var Character = builtinclass{[]ilos.Class{Object}, []string{}, "<CHARACTER>"}
var Function = builtinclass{[]ilos.Class{Object}, []string{}, "<FUNCTION>"}
var GenericFunction = builtinclass{[]ilos.Class{Function}, []string{}, "<GENERIC-FUNCTION>"}
var StandardGenericFunction = builtinclass{[]ilos.Class{GenericFunction}, []string{}, "<STANDARD-GENERIC-FUNCTION>"}
var List = builtinclass{[]ilos.Class{Object}, []string{}, "<LIST>"}
var Cons = builtinclass{[]ilos.Class{List}, []string{}, "<CONS>"}
var Null = builtinclass{[]ilos.Class{List}, []string{}, "<NULL>"}
var Symbol = builtinclass{[]ilos.Class{Object}, []string{}, "<SYMBOL>"}
var Number = builtinclass{[]ilos.Class{Object}, []string{}, "<NUMBER>"}
var Integer = builtinclass{[]ilos.Class{Number}, []string{}, "<INTEGER>"}
var Float = builtinclass{[]ilos.Class{Number}, []string{}, "<FLOAT>"}

var SeriousCondition = builtinclass{[]ilos.Class{Object}, []string{}, "<SERIOUS-CONDITION>"}
var Error = builtinclass{[]ilos.Class{SeriousCondition}, []string{}, "<ERROR>"}
var ArithmeticError = builtinclass{[]ilos.Class{Error}, []string{"OPERATION", "OPERANDS"}, "<ARITHMETIC-ERROR>"}
var DivisionByZero = builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<DIVISION-BY-ZERO>"}
var FloatingPointOnderflow = builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<FLOATING-POINT-OVERFLOW>"}
var FloatingPointUnderflow = builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<FLOATING-POINT-UNDERFLOW>"}
var ControlError = builtinclass{[]ilos.Class{Error}, []string{}, "<CONTROL-ERROR>"}
var ParseError = builtinclass{[]ilos.Class{Error}, []string{"STRING", "EXPECTED-CLASS"}, "<PARSE-ERROR>"}
var ProgramError = builtinclass{[]ilos.Class{Error}, []string{}, "<PROGRAM-ERROR>"}
var DomainError = builtinclass{[]ilos.Class{ProgramError}, []string{"OBJECT", "EXPECTED-CLASS"}, "<DOMAIN-ERROR>"}
var UndefinedEntity = builtinclass{[]ilos.Class{ProgramError}, []string{"NAME", "NAMESPACE"}, "<UNDEFINED-ENTITY>"}
var UndefinedVariable = builtinclass{[]ilos.Class{UndefinedEntity}, []string{}, "<UNDEFINED-VARIABLE>"}
var UndefinedFunction = builtinclass{[]ilos.Class{UndefinedEntity}, []string{}, "<UNDEFINED-FUNCTION>"}
var SimpleError = builtinclass{[]ilos.Class{Error}, []string{"FORMAT-STRING", "FORMAT-ARGUMENTS"}, "<SIMPLE-ERROR>"}
var StreamError = builtinclass{[]ilos.Class{Error}, []string{}, "<STREAM-ERROR>"}
var EndOfStream = builtinclass{[]ilos.Class{StreamError}, []string{}, "<END-OF-STREAM>"}
var StorageExhausted = builtinclass{[]ilos.Class{SeriousCondition}, []string{}, "<STORAGE-EXHAUSTED>"}
var StandardObject = builtinclass{[]ilos.Class{Object}, []string{}, "<STANDARD-OBJECT>"}
var Stream = builtinclass{[]ilos.Class{Object}, []string{"STREAM"}, "<STREAM>"}

// Implementation defined
var Escape = builtinclass{[]ilos.Class{Object}, []string{"TAG"}, "<ESCAPE>"}
var CatchTag = builtinclass{[]ilos.Class{Escape}, []string{"OBJECT"}, "<THROW>"}
var TagbodyTag = builtinclass{[]ilos.Class{Escape}, []string{}, "<TAGBODY-TAG>"}
var BlockTag = builtinclass{[]ilos.Class{Escape}, []string{"OBJECT"}, "<BLOCK-TAG>"}
