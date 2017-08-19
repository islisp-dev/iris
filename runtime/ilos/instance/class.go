// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package instance

import (
	"github.com/ta2gch/iris/runtime/ilos"
)

var ObjectClass = NewBuiltInClass("<OBJECT>", nil)
var BuiltInClassClass = NewBuiltInClass("<BUILT-IN-CLASS>", ObjectClass)
var StandardClassClass = NewBuiltInClass("<STANDARD-CLASS>", ObjectClass)
var BasicArrayClass = NewBuiltInClass("<BASIC-ARRAY>", ObjectClass)
var BasicArrayStarClass = NewBuiltInClass("<BASIC-ARRAY*>", BasicArrayClass)
var GeneralArrayStarClass = NewBuiltInClass("<GENERAL-ARRAY*>", BasicArrayStarClass)
var BasicVectorClass = NewBuiltInClass("<BASIC-VECTOR>", BasicArrayClass)
var GeneralVectorClass = NewBuiltInClass("<GENERAL-VECTOR>", BasicVectorClass)
var StringClass = NewBuiltInClass("<STRING>", BasicVectorClass)
var CharacterClass = NewBuiltInClass("<CHARACTER>", ObjectClass)
var FunctionClass = NewBuiltInClass("<FUNCTION>", ObjectClass)
var GenericFunctionClass = NewBuiltInClass("<GENERIC-FUNCTION>", FunctionClass)
var StandardGenericFunctionClass = NewBuiltInClass("<STANDARD-GENERIC-FUNCTION>", GenericFunctionClass)
var ListClass = NewBuiltInClass("<LIST>", ObjectClass)
var ConsClass = NewBuiltInClass("<CONS>", ListClass)
var NullClass = BuiltInClass{NewSymbol("<NULL>"), []ilos.Class{ListClass, SymbolClass}, []ilos.Instance{}}
var SymbolClass = NewBuiltInClass("<SYMBOL>", ObjectClass)
var NumberClass = NewBuiltInClass("<NUMBER>", ObjectClass)
var IntegerClass = NewBuiltInClass("<INTEGER>", NumberClass)
var FloatClass = NewBuiltInClass("<FLOAT>", NumberClass)

var SeriousConditionClass = NewBuiltInClass("<SERIOUS-CONDITION>", ObjectClass)
var ErrorClass = NewBuiltInClass("<ERROR>", SeriousConditionClass)
var ArithmeticErrorClass = NewBuiltInClass("<ARITHMETIC-ERROR>", ErrorClass, "OPERATION", "OPERANDS")
var DivisionByZeroClass = NewBuiltInClass("<DIVISION-BY-ZERO>", ArithmeticErrorClass)
var FloatingPointOnderflowClass = NewBuiltInClass("<FLOATING-POINT-OVERFLOW>", ArithmeticErrorClass)
var FloatingPointUnderflowClass = NewBuiltInClass("<FLOATING-POINT-UNDERFLOW>", ArithmeticErrorClass)
var ControlErrorClass = NewBuiltInClass("<CONTROL-ERROR>", ErrorClass)
var ParseErrorClass = NewBuiltInClass("<PARSE-ERROR>", ErrorClass, "STRING", "EXPECTED-CLASS")
var ProgramErrorClass = NewBuiltInClass("<PROGRAM-ERROR>", ErrorClass)
var DomainErrorClass = NewBuiltInClass("<DOMAIN-ERROR>", ProgramErrorClass, "OBJECT", "EXPECTED-CLASS")
var UndefinedEntityClass = NewBuiltInClass("<UNDEFINED-ENTITY>", ProgramErrorClass, "NAME", "NAMESPACE")
var UndefinedVariableClass = NewBuiltInClass("<UNDEFINED-VARIABLE>", UndefinedEntityClass)
var UndefinedFunctionClass = NewBuiltInClass("<UNDEFINED-FUNCTION>", UndefinedEntityClass)
var SimpleErrorClass = NewBuiltInClass("<SIMPLE-ERROR>", ErrorClass, "FORMAT-STRING", "FORMAT-ARGUMENTS")
var StreamErrorClass = NewBuiltInClass("<STREAM-ERROR>", ErrorClass)
var EndOfStreamClass = NewBuiltInClass("<END-OF-STREAM>", StreamErrorClass)
var StorageExhaustedClass = NewBuiltInClass("<STORAGE-EXHAUSTED>", SeriousConditionClass)
var StandardObjectClass = NewBuiltInClass("<STANDARD-OBJECT>", ObjectClass)
var StreamClass = NewBuiltInClass("<STREAM>", ObjectClass, "STREAM")

// Implementation defined
var EscapeClass = NewBuiltInClass("<ESCAPE>", ObjectClass, "TAG")
var CatchTagClass = NewBuiltInClass("<THROW>", EscapeClass, "OBJECT")
var TagbodyTagClass = NewBuiltInClass("<TAGBODY-TAG>", EscapeClass)
var BlockTagClass = NewBuiltInClass("<BLOCK-TAG>", EscapeClass, "OBJECT")
