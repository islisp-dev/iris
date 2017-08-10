package runtime

import (
	"math"

	env "github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func lambda(local, global *env.Environment, lambdaList ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// lambdaFunction-list must be a instance of list and ends with nil
	if !instance.Of(class.List, lambdaList) || !UnsafeEndOfListIsNil(lambdaList) { // Checked at the head of test
		return nil, instance.New(class.ParseError, lambdaList, class.List)
	}
	lexical := local
	cdr := lambdaList
	cnt := 0
	min := 0
	max := 0
	for instance.Of(class.Cons, cdr) {
		cadr := instance.UnsafeCar(cdr)
		cddr := instance.UnsafeCdr(cdr)
		cnt++
		if cadr == instance.New(class.Symbol, ":REST") || cadr == instance.New(class.Symbol, "&REST") {
			min = cnt
			max = math.MaxInt64
			break
		}
		min = cnt
		max = cnt
		cdr = cddr
	}
	return instance.New(class.Function, instance.New(class.Symbol, "ANONYMOUS-FUNCTION"), func(local, global *env.Environment, args ilos.Instance) (ilos.Instance, ilos.Instance) {
		local.BlockTag = append(lexical.BlockTag, local.BlockTag...)
		local.TagbodyTag = append(lexical.TagbodyTag, local.TagbodyTag...)
		local.CatchTag = append(lexical.CatchTag, local.CatchTag...)
		local.Variable = append(lexical.Variable, local.Variable...)
		local.Function = append(lexical.Function, local.Function...)
		local.Macro = append(lexical.Macro, local.Macro...)
		local.DynamicVariable = append(lexical.DynamicVariable, local.DynamicVariable...)
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
		ret := instance.New(class.Null)
		var err ilos.Instance
		for _, form := range forms {
			ret, err = Eval(local, global, form)
			if err != nil {
				return nil, err
			}
		}
		return ret, nil
	}, min, max), nil
}
