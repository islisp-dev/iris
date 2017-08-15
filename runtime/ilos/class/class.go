// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package class

import "github.com/ta2gch/iris/runtime/ilos"

func Is(c, p ilos.Class) bool {
	var sub func(c, p ilos.Class) bool
	sub = func(c, p ilos.Class) bool {
		if c == p {
			return true
		}
		for _, d := range c.Parents() {
			if sub(d, p) {
				return true
			}
		}
		return false
	}
	for _, d := range c.Parents() {
		if sub(d, p) {
			return true
		}
	}
	return false
}

var Object = &primitiveclass{[]ilos.Class{}, "<OBJECT>"}
var BuiltInClass = &primitiveclass{[]ilos.Class{Object}, "<BUILT-IN-CLASS>"}
var StandardClass = &primitiveclass{[]ilos.Class{Object}, "<STANDARD-CLASS>"}
var BasicArray = &primitiveclass{[]ilos.Class{Object}, "<BASIC-ARRAY>"}
var BasicArrayStar = &primitiveclass{[]ilos.Class{BasicArray}, "<BASIC-ARRAY*>"}
var GeneralArrayStar = &primitiveclass{[]ilos.Class{BasicArrayStar}, "<GENERAL-ARRAY*>"}
var BasicVector = &primitiveclass{[]ilos.Class{BasicArray}, "<BASIC-VECTOR>"}
var GeneralVector = &primitiveclass{[]ilos.Class{BasicVector}, "<GENERAL-VECTOR>"}
var String = &primitiveclass{[]ilos.Class{BasicVector}, "<STRING>"}
var Character = &primitiveclass{[]ilos.Class{Object}, "<CHARACTER>"}
var Function = &primitiveclass{[]ilos.Class{Object}, "<FUNCTION>"}
var GenericFunction = &primitiveclass{[]ilos.Class{Function}, "<GENERIC-FUNCTION>"}
var StandardGenericFunction = &primitiveclass{[]ilos.Class{GenericFunction}, "<STANDARD-GENERIC-FUNCTION>"}
var List = &primitiveclass{[]ilos.Class{Object}, "<LIST>"}
var Cons = &primitiveclass{[]ilos.Class{List}, "<CONS>"}
var Null = &primitiveclass{[]ilos.Class{List}, "<NULL>"}
var Symbol = &primitiveclass{[]ilos.Class{Object}, "<SYMBOL>"}
var Number = &primitiveclass{[]ilos.Class{Object}, "<NUMBER>"}
var Integer = &primitiveclass{[]ilos.Class{Number}, "<INTEGER>"}
var Float = &primitiveclass{[]ilos.Class{Number}, "<FLOAT>"}

var SeriousCondition = &builtinclass{[]ilos.Class{Object}, []string{}, "<SERIOUS-CONDITION>"}
var Error = &builtinclass{[]ilos.Class{SeriousCondition}, []string{}, "<ERROR>"}
var ArithmeticError = &builtinclass{[]ilos.Class{Error}, []string{"OPERATION", "OPERANDS"}, "<ARITHMETIC-ERROR>"}
var DivisionByZero = &builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<DIVISION-BY-ZERO>"}
var FloatingPointOnderflow = &builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<FLOATING-POINT-OVERFLOW>"}
var FloatingPointUnderflow = &builtinclass{[]ilos.Class{ArithmeticError}, []string{}, "<FLOATING-POINT-UNDERFLOW>"}
var ControlError = &builtinclass{[]ilos.Class{Error}, []string{}, "<CONTROL-ERROR>"}
var ParseError = &builtinclass{[]ilos.Class{Error}, []string{"STRING", "EXPECTED-CLASS"}, "<PARSE-ERROR>"}
var ProgramError = &builtinclass{[]ilos.Class{Error}, []string{"CAUSE"}, "<PROGRAM-ERROR>"}
var DomainError = &builtinclass{[]ilos.Class{ProgramError}, []string{"OBJECT", "EXPECTED-CLASS"}, "<DOMAIN-ERROR>"}
var UndefinedEntity = &builtinclass{[]ilos.Class{ProgramError}, []string{"NAME", "NAMESPACE"}, "<UNDEFINED-ENTITY>"}
var UndefinedVariable = &builtinclass{[]ilos.Class{UndefinedEntity}, []string{}, "<UNDEFINED-VARIABLE>"}
var UndefinedFunction = &builtinclass{[]ilos.Class{UndefinedEntity}, []string{}, "<UNDEFINED-FUNCTION>"}
var SimpleError = &builtinclass{[]ilos.Class{Error}, []string{"FORMAT-STRING", "FORMAT-ARGUMENTS"}, "<SIMPLE-ERROR>"}
var StreamError = &builtinclass{[]ilos.Class{Error}, []string{}, "<STREAM-ERROR>"}
var EndOfStream = &builtinclass{[]ilos.Class{StreamError}, []string{}, "<END-OF-STREAM>"}
var StorageExhausted = &builtinclass{[]ilos.Class{SeriousCondition}, []string{}, "<STORAGE-EXHAUSTED>"}
var StandardObject = &builtinclass{[]ilos.Class{Object}, []string{}, "<STANDARD-OBJECT>"}
var Stream = &builtinclass{[]ilos.Class{Object}, []string{"STREAM"}, "<STREAM>"}

// Implementation defined
var Escape = &builtinclass{[]ilos.Class{Object}, []string{"TAG"}, "<ESCAPE>"}
var CatchTag = &builtinclass{[]ilos.Class{Escape}, []string{"OBJECT"}, "<THROW>"}
var TagbodyTag = &builtinclass{[]ilos.Class{Escape}, []string{}, "<TAGBODY-TAG>"}
var BlockTag = &builtinclass{[]ilos.Class{Escape}, []string{"OBJECT"}, "<BLOCK-TAG>"}
