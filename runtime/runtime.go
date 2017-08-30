// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

package runtime

import (
	"math"
	"os"
	"regexp"
	"strings"

	"github.com/ta2gch/iris/runtime/env"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

var TopLevel = env.NewEnvironment(instance.NewStream(os.Stdin, nil), instance.NewStream(nil, os.Stdout), instance.NewStream(nil, os.Stderr), nil)
var Version = "0.1.0"

func defspecial2(name string, function interface{}) {
	name = regexp.MustCompile(`(.)([A-Z])`).ReplaceAllString(name, "$1-$2")
	name = strings.ToUpper(name)
	symbol := instance.NewSymbol(name)
	TopLevel.Special.Define(symbol, instance.NewFunction(func2symbol(function), function))
}

func defun2(name string, function interface{}) {
	name = regexp.MustCompile(`(.)([A-Z])`).ReplaceAllString(name, "$1-$2")
	name = strings.ToUpper(name)
	symbol := instance.NewSymbol(name)
	TopLevel.Function.Define(symbol, instance.NewFunction(symbol, function))
}
func defglobal(name string, value ilos.Instance) {
	name = regexp.MustCompile(`(.)([A-Z])`).ReplaceAllString(name, "$1-$2")
	name = strings.ToUpper(name)
	symbol := instance.NewSymbol(name)
	TopLevel.Variable.Define(symbol, value)
}
func init() {
	defglobal("*pi*", instance.Float(math.Pi))
	defglobal("*MostPositiveFloat*", MostPositiveFloat)
	defglobal("*MostNegativeFloat*", MostNegativeFloat)
	defun2("-", Substruct)
	defun2("+", Add)
	defun2("*", Multiply)
	defun2("<", NumberLessThan)
	defun2("<=", NumberLessThanOrEqual)
	defun2("=", NumberEqual)
	defun2(">", NumberGreaterThan)
	defun2(">=", NumberGreaterThanOrEqual)
	defspecial2("Quasiquote", Quasiquote)
	defun2("Abs", Abs)
	defspecial2("And", And)
	defun2("Append", Append)
	defun2("Apply", Apply)
	defun2("Aref", Aref)
	defun2("Assoc", Assoc)
	// TODO: defspecial2("Assure", Assure)
	defun2("Atan", Atan)
	defun2("Atan2", Atan2)
	defun2("Atanh", Atanh)
	defun2("BasicArray*P", BasicArrayStarP)
	defun2("BasicArrayP", BasicArrayP)
	defun2("BasicVectorP", BasicVectorP)
	defspecial2("Block", Block)
	defun2("Car", Car)
	defspecial2("Case", Case)
	defspecial2("CaseUsing", CaseUsing)
	defspecial2("Catch", Catch)
	defun2("Cdr", Cdr)
	defun2("Ceiling", Ceiling)
	defun2("Cerror", Cerror)
	defun2("CharIndex", CharIndex)
	defun2("char/=", CharNotEqual)
	defun2("Char<", CharLessThan)
	defun2("Char<=", CharLessThanOrEqual)
	defun2("Char=", CharEqual)
	defun2("Char>", CharGreaterThan)
	defun2("Char>=", CharGreaterThanOrEqual)
	defun2("Characterp", Characterp)
	defspecial2("Class", Class)
	defun2("ClassOf", ClassOf)
	defun2("Close", Close)
	// TODO defun2("Coercion", Coercion)
	defspecial2("Cond", Cond)
	defun2("ConditionContinuable", ConditionContinuable)
	defun2("Cons", Cons)
	defun2("Consp", Consp)
	defun2("ContinueCondition", ContinueCondition)
	// TODO defun2("Convert", Convert)
	defun2("Cos", Cos)
	defun2("Cosh", Cosh)
	defun2("Create", Create) //TODO Change to generic function
	defun2("CreateArray", CreateArray)
	defun2("CreateList", CreateList)
	defun2("CreateString", CreateString)
	defun2("CreateStringInputStream", CreateStringInputStream)
	defun2("CreateStringOutputStream", CreateStringOutputStream)
	defun2("CreateVector", CreateVector)
	defspecial2("Defclass", Defclass)
	defspecial2("Defconstant", Defconstant)
	defspecial2("Defdynamic", Defdynamic)
	defspecial2("Defgeneric", Defgeneric)
	defspecial2("Defmethod", Defmethod)
	defspecial2("Defglobal", Defglobal)
	defspecial2("Defmacro", Defmacro)
	defspecial2("Defun", Defun)
	defun2("Div", Div)
	defspecial2("Dynamic", Dynamic)
	defspecial2("DynamicLet", DynamicLet)
	defun2("Elt", Elt)
	defun2("Eq", Eq)
	defun2("Eql", Eql)
	defun2("Equal", Equal)
	defun2("Error", Error)
	defun2("ErrorOutput", ErrorOutput)
	defun2("Exp", Exp)
	defun2("Expt", Expt)
	// TODO defun2("FileLength", FileLength)
	// TODO defun2("FilePosition", FilePosition)
	// TODO defun2("FinishOutput", FinishOutput)
	defspecial2("Flet", Flet)
	defun2("Float", Float)
	defun2("Floatp", Floatp)
	defun2("Floor", Floor)
	defspecial2("For", For)
	defun2("Format", Format) // TODO full syntax
	// TODO other print function
	defun2("Funcall", Funcall)
	defspecial2("Function", Function)
	defun2("Functionp", Functionp)
	defun2("Garef", Garef)
	defun2("Gcd", Gcd)
	defun2("GENERAL-ARRAY*-P", GeneralArrayStarP)
	defun2("GeneralVectorP", GeneralVectorP)
	// TODO defun2("GenericFunctionP", GenericFunctionP)
	defun2("Gensym", Gensym)
	// TODO defun2("GetInternalRealTime", GetInternalRealTime)
	// TODO defun2("GetInternalRunTime", GetInternalRunTime)
	defun2("GetOutputStreamString", GetOutputStreamString)
	// TODO defun2("GetUniversalTime", GetUniversalTime)
	defspecial2("Go", Go)
	// TODO defun2("Identity", Identity)
	defspecial2("If", If)
	// TODO defspecial2("IgnoreErrors", IgnoreErrors)
	defun2("InitializeObject", InitializeObject) // TODO change generic function
	defun2("InputStreamP", InputStreamP)
	defun2("Instancep", Instancep)
	// TODO defun2("Integer", Integer)
	defun2("Integerp", Integerp)
	// TODO defun2("InternalTimeUnitsPerSecond", InternalTimeUnitsPerSecond)
	defun2("Isqrt", Isqrt)
	defspecial2("Labels", Labels)
	defspecial2("Lambda", Lambda)
	defun2("Lcm", Lcm)
	defun2("Length", Length)
	defspecial2("Let", Let)
	defspecial2("LET*", LetStar)
	defun2("List", List)
	defun2("Listp", Listp)
	defun2("Log", Log)
	defun2("MapInto", MapInto)
	defun2("Mapc", Mapc)
	defun2("Mapcan", Mapcan)
	defun2("Mapcar", Mapcar)
	defun2("Mapcon", Mapcon)
	defun2("Mapl", Mapl)
	defun2("Maplist", Maplist)
	defun2("Max", Max)
	defun2("Member", Member)
	defun2("Min", Min)
	defun2("Mod", Mod)
	defglobal("NIL", Nil)
	defun2("Not", Not)
	defun2("Nreverse", Nreverse)
	defun2("Null", Null)
	defun2("Numberp", Numberp)
	defun2("OpenInputFile", OpenInputFile)
	defun2("OpenIoFile", OpenIoFile)
	defun2("OpenOutputFile", OpenOutputFile)
	defun2("OpenStreamP", OpenStreamP)
	defspecial2("Or", Or)
	defun2("OutputStreamP", OutputStreamP)
	defun2("ParseNumber", ParseNumber)
	// TODO defun2("PreviewChar", PreviewChar)
	// TODO defun2("ProveFile", ProveFile)
	defspecial2("Progn", Progn)
	defun2("Property", Property)
	defspecial2("Quasiquote", Quasiquote)
	defspecial2("Quote", Quote)
	defun2("Quotient", Quotient)
	defun2("Read", Read)
	// TODO defun2("ReadByte", ReadByte)
	defun2("ReadChar", ReadChar)
	defun2("ReadLine", ReadLine)
	defun2("RemoveProperty", RemoveProperty)
	defun2("ReportCondition", ReportCondition)
	defspecial2("ReturnFrom", ReturnFrom)
	defun2("Reverse", Reverse)
	defun2("Round", Round)
	defun2("SetAref", SetAref)
	defun2("(setf aref)", SetAref)
	defun2("SetCar", SetCar)
	defun2("(setf car)", SetCar)
	defun2("SetCdr", SetCdr)
	defun2("(setf cdr)", SetCdr)
	defun2("SetDynamic", SetDynamic)
	defun2("(setf dynamic)", SetDynamic)
	defun2("SetElt", SetElt)
	defun2("(setf elt)", SetElt)
	// TODO defun2("SetFilePosition", SetFilePosition)
	defun2("SetGaref", SetGaref)
	defun2("(setf garef)", SetGaref)
	defun2("SetProperty", SetProperty)
	defun2("(setf property)", SetProperty)
	defspecial2("Setf", Setf)
	defspecial2("Setq", Setq)
	defun2("SignalCondition", SignalCondition)
	// TODO defun2("SimpleErrorFormatArguments", SimpleErrorFormatArguments)
	// TODO defun2("SimpleErrorFormatString", SimpleErrorFormatString)
	defun2("Sin", Sin)
	defun2("Sinh", Sinh)
	defun2("Sqrt", Sqrt)
	defun2("StandardInput", StandardInput)
	defun2("StandardOutput", StandardOutput)
	defun2("StreamReadyP", StreamReadyP)
	defun2("Streamp", Streamp)
	defun2("StringAppend", StringAppend)
	defun2("StringIndex", StringIndex)
	defun2("String/=", StringNotEqual)
	defun2("String<", StringGreaterThan)
	defun2("String<=", StringGreaterThan)
	defun2("String=", StringEqual)
	defun2("String>", StringGreaterThan)
	defun2("String>=", StringGreaterThanOrEqual)
	defun2("Stringp", Stringp)
	defun2("Subclassp", Subclassp)
	defun2("Subseq", Subseq)
	defun2("Symbolp", Symbolp)
	defglobal("T", T)
	defspecial2("Tagbody", Tagbody)
	defspecial2("Tan", Tan)
	defspecial2("Tanh", Tanh)
	// TODO defspecial2("The", The)
	defspecial2("Throw", Throw)
	defun2("Truncate", Truncate)
	// TODO defun2("UndefinedEntityName", UndefinedEntityName)
	// TODO defun2("UndefinedEntityNamespace", UndefinedEntityNamespace)
	defspecial2("UnwindProtect", UnwindProtect)
	defun2("Vector", Vector)
	defspecial2("While", While)
	defspecial2("WithErrorOutput", WithErrorOutput)
	defspecial2("WithHandler", WithHandler)
	defspecial2("WithOpenInputFile", WithOpenInputFile)
	defspecial2("WithOpenOutputFile", WithOpenOutputFile)
	defspecial2("WithStandardInput", WithStandardInput)
	defspecial2("WithStandardOutput", WithStandardOutput)
	// TODO defun2("WriteByte", WriteByte)
}