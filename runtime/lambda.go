package runtime

import (
	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func lambda(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
	// args must be a instance of Cons, not Null, and ends with nil
	if !ilos.InstanceOf(args, class.Cons) || !UnsafeEndOfListIsNil(args) { // Checked at the head of test
		return nil, instance.NewWrongNumberOfArguments(instance.NewSymbol("LAMBDA"), args)
	}
	lambdaList := instance.UnsafeCar(args)
	// lambda-list must be a instance of list and ends with nil
	if !ilos.InstanceOf(lambdaList, class.List) || !UnsafeEndOfListIsNil(lambdaList) { // Checked at the head of test
		return nil, instance.NewParseError(lambdaList, class.List)
	}
	forms := instance.UnsafeCdr(args) // Checked at the top of this function. (EndOfListIsNil)
	lexical := local
	return instance.NewFunction(func(args ilos.Instance, local *env.Environment, global *env.Environment) (ilos.Instance, ilos.Instance) {
		local.TagBodyTag = append(lexical.TagBodyTag, local.TagBodyTag...)
		local.ThrowTag = append(lexical.ThrowTag, local.ThrowTag...)
		local.Variable = append(lexical.Variable, local.Variable...)
		local.Function = append(lexical.Function, local.Function...)
		local.Macro = append(lexical.Macro, local.Macro...)
		local.DynamicVariable = append(lexical.DynamicVariable, local.DynamicVariable...)
		// args must be a instance of list and end with nil
		if !ilos.InstanceOf(args, class.List) || !UnsafeEndOfListIsNil(args) { // Checked at the head of test
			return nil, instance.NewParseError(args, class.List)
		}
		cdr := lambdaList
		ok := false
		for ilos.InstanceOf(cdr, class.Cons) {
			car := instance.UnsafeCar(cdr) // Checked at the top of this loop.
			cdr = instance.UnsafeCdr(cdr)  // Checked at the top of this loop.
			if car == instance.NewSymbol(":REST") || car == instance.NewSymbol("&REST") {
				// fargs has only one symbol after &rest or :rest symbol.
				if ilos.InstanceOf(cdr, class.List) && UnsafeListLength(cdr) == 1 { // Checked at the head of test
					// If fargs has :rest or &rest symbol, The length of aargs must be greater than or equal to 'the length of fargs' - 2.
					// aargs is a instance of list and ends with nil becauseof the checking at this function.
					// lambda-list is a instance of list and ends with nil becauseof the checking at the function, lambda.
					if UnsafeListLength(lambdaList)-2 <= UnsafeListLength(args) {
						ok = true
						break
					} else {
						return nil, instance.NewWrongNumberOfArguments(instance.NewSymbol("ANONYMOUS-FUNCTION"), args)
					}
				} else {
					return nil, instance.NewWrongNumberOfArguments(instance.NewSymbol("LAMBDA"), lambdaList)
				}
			}
		}
		// If fargs doesn't have them, The length of aargs must be equal to the length of fargs.
		// aargs is a instance of list and ends nil becauseof the checking at this function.
		// lambda-list is a instance of list and ends nil becauseof the checking at the function, lambda.
		if !ok && UnsafeListLength(lambdaList) != UnsafeListLength(args) {
			return nil, instance.NewWrongNumberOfArguments(instance.NewSymbol("ANONYMOUS-FUNCTION"), args)
		}
		fargs := lambdaList
		aargs := args
		for ilos.InstanceOf(fargs, class.Cons) && ilos.InstanceOf(aargs, class.Cons) {
			key := instance.UnsafeCar(fargs)   // Checked at the top of this loop.
			value := instance.UnsafeCar(aargs) // Checked at the top of this loop.
			if key == instance.NewSymbol(":REST") || key == instance.NewSymbol("&REST") {
				cadr := instance.UnsafeCar(instance.UnsafeCdr(fargs)) // Checked before type checking secion
				local.Variable.Define(cadr, aargs)
				break
			}
			local.Variable.Define(key, value)
			fargs = instance.UnsafeCdr(fargs) // Checked at the top of this loop
			aargs = instance.UnsafeCdr(aargs) // Checked at the top of this loop
		}
		body := forms
		ret := instance.NewNull()
		var err ilos.Instance
		for ilos.InstanceOf(body, class.Cons) {
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
