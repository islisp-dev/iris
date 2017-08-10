package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func lambda(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !instance.Of(class.Cons, args) || !UnsafeEndOfListIsNil(args) { // Checked at the head of test
		return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
			"FORM":      instance.New(class.Symbol, "LAMBDA"),
			"ARGUMENTS": args,
		})
	}
	lambdaList := instance.UnsafeCar(args)
	// lambda-list must be a instance of list and ends with nil
	if !instance.Of(class.List, lambdaList) || !UnsafeEndOfListIsNil(lambdaList) { // Checked at the head of test
		return nil, instance.New(class.ParseError, lambdaList, class.List)
	}
	forms := instance.UnsafeCdr(args) // Checked at the top of this function. (EndOfListIsNil)
	lexical := local
	return instance.New(class.Function, func(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
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
		cdr := lambdaList
		ok := false
		for instance.Of(class.Cons, cdr) {
			car := instance.UnsafeCar(cdr) // Checked at the top of this loop.
			cdr = instance.UnsafeCdr(cdr)  // Checked at the top of this loop.
			if car == instance.New(class.Symbol, ":REST") || car == instance.New(class.Symbol, "&REST") {
				// fargs has only one symbol after &rest or :rest symbol.
				if instance.Of(class.List, cdr) && UnsafeListLength(cdr) == 1 { // Checked at the head of test
					// If fargs has :rest or &rest symbol, The length of aargs must be greater than or equal to 'the length of fargs' - 2.
					// aargs is a instance of list and ends with nil becauseof the checking at this function.
					// lambda-list is a instance of list and ends with nil becauseof the checking at the function, lambda.
					if UnsafeListLength(lambdaList)-2 <= UnsafeListLength(args) {
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
						"FORM":      instance.New(class.Symbol, "LAMBDA"),
						"ARGUMENTS": lambdaList,
					})
				}
			}
		}
		// If fargs doesn't have them, The length of aargs must be equal to the length of fargs.
		// aargs is a instance of list and ends nil becauseof the checking at this function.
		// lambda-list is a instance of list and ends nil becauseof the checking at the function, lambda.
		if !ok && UnsafeListLength(lambdaList) != UnsafeListLength(args) {
			return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
				"FORM":      instance.New(class.Symbol, "ANONYMOUS-FUNCTION"),
				"ARGUMENTS": args,
			})
		}
		fargs := lambdaList
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
		body := forms
		ret := instance.New(class.Null)
		var err ilos.Instance
		for instance.Of(class.Cons, body) {
			exp := instance.UnsafeCar(body) // Checked at the top of this loop
			ret, err = Eval(exp, local, global)
			if err != nil {
				return nil, err
			}
			body = instance.UnsafeCdr(body) // Checked at the top of this loop
		}
		return ret, nil
	}), nil
}
