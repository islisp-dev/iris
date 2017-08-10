package runtime

import (
	"fmt"

	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

type lambdaFunction func(*env.Environment, *env.Environment, ilos.Instance) (ilos.Instance, ilos.Instance)

func (lambdaFunction) Class() ilos.Class {
	return class.Function
}

func (lambdaFunction) GetSlotValue(key ilos.Instance, _ ilos.Class) (ilos.Instance, bool) {
	return nil, false
}

func (lambdaFunction) SetSlotValue(key ilos.Instance, value ilos.Instance, _ ilos.Class) bool {
	return false
}

func (f lambdaFunction) String() string {
	return fmt.Sprintf("#%v", f.Class())
}

func (f lambdaFunction) Apply(args ilos.Instance, local, global *env.Environment) (ilos.Instance, ilos.Instance) {
	a, b := f(local, global, args)
	return a, b
}

func lambda(local, global *env.Environment, lambdaFunctionList ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// lambdaFunction-list must be a instance of list and ends with nil
	if !instance.Of(class.List, lambdaFunctionList) || !UnsafeEndOfListIsNil(lambdaFunctionList) { // Checked at the head of test
		return nil, instance.New(class.ParseError, lambdaFunctionList, class.List)
	}
	lexical := local
	return lambdaFunction(func(local, global *env.Environment, args ilos.Instance) (ilos.Instance, ilos.Instance) {
		local.BlockTag = append(lexical.BlockTag, local.BlockTag...)
		local.TagbodyTag = append(lexical.TagbodyTag, local.TagbodyTag...)
		local.CatchTag = append(lexical.CatchTag, local.CatchTag...)
		local.Variable = append(lexical.Variable, local.Variable...)
		local.Function = append(lexical.Function, local.Function...)
		local.Macro = append(lexical.Macro, local.Macro...)
		local.DynamicVariable = append(lexical.DynamicVariable, local.DynamicVariable...)
		// args must be a instance of list and end with nil
		if !instance.Of(class.List, args) || !UnsafeEndOfListIsNil(args) { // Checked at the head of test
			return nil, instance.New(class.DomainError, map[string]ilos.Instance{
				"OBJECT":         args,
				"EXPECTED-CLASS": class.List,
			})
		}
		cdr := lambdaFunctionList
		ok := false
		for instance.Of(class.Cons, cdr) {
			car := instance.UnsafeCar(cdr) // Checked at the top of this loop.
			cdr = instance.UnsafeCdr(cdr)  // Checked at the top of this loop.
			if car == instance.New(class.Symbol, ":REST") || car == instance.New(class.Symbol, "&REST") {
				// fargs has only one symbol after &rest or :rest symbol.
				if instance.Of(class.List, cdr) && UnsafeListLength(cdr) == 1 { // Checked at the head of test
					// If fargs has :rest or &rest symbol, The length of aargs must be greater than or equal to 'the length of fargs' - 2.
					// aargs is a instance of list and ends with nil becauseof the checking at this function.
					// lambdaFunction-list is a instance of list and ends with nil becauseof the checking at the function, lambdaFunction.
					if UnsafeListLength(lambdaFunctionList)-2 <= UnsafeListLength(args) {
						ok = true
						break
					} else {
						return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
							"FORM":      instance.New(class.Symbol, "ANONYMOUS-FUNCTION"),
							"ARGUMENTS": args,
						})
					}
				} else {
					return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
						"FORM":      instance.New(class.Symbol, "lambdaFunction"),
						"ARGUMENTS": lambdaFunctionList,
					})
				}
			}
		}
		// If fargs doesn't have them, The length of aargs must be equal to the length of fargs.
		// aargs is a instance of list and ends nil becauseof the checking at this function.
		// lambdaFunction-list is a instance of list and ends nil becauseof the checking at the function, lambdaFunction.
		if !ok && UnsafeListLength(lambdaFunctionList) != UnsafeListLength(args) {
			return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
				"FORM":      instance.New(class.Symbol, "ANONYMOUS-FUNCTION"),
				"ARGUMENTS": args,
			})
		}
		fargs := lambdaFunctionList
		aargs := args
		for instance.Of(class.Cons, fargs) && instance.Of(class.Cons, aargs) {
			key := instance.UnsafeCar(fargs)   // Checked at the top of this loop.
			value := instance.UnsafeCar(aargs) // Checked at the top of this loop.
			if key == instance.New(class.Symbol, ":REST") || key == instance.New(class.Symbol, "&REST") {
				cadr := instance.UnsafeCar(instance.UnsafeCdr(fargs)) // Checked before type checking secion
				local.Variable.Define(cadr, aargs)
				break
			}
			local.Variable.Define(key, value)
			fargs = instance.UnsafeCdr(fargs) // Checked at the top of this loop
			aargs = instance.UnsafeCdr(aargs) // Checked at the top of this loop
		}
		ret := instance.New(class.Null)
		var err ilos.Instance
		for _, form := range forms {
			ret, err = Eval(local, global, form)
			if err != nil {
				return nil, err
			}
		}
		return ret, nil
	}), nil
}
