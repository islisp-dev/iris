package runtime

import (
	"github.com/ta2gch/iris/runtime/environment"
	"github.com/ta2gch/iris/runtime/ilos"
	"github.com/ta2gch/iris/runtime/ilos/class"
	"github.com/ta2gch/iris/runtime/ilos/instance"
)

func lambda(local, global *environment.Environment, lambdaList ilos.Instance, forms ...ilos.Instance) (ilos.Instance, ilos.Instance) {
	// lambdaFunction-list must be a instance of list and ends with nil
	if !instance.Of(class.List, lambdaList) || !UnsafeEndOfListIsNil(lambdaList) { // Checked at the head of test
		return nil, instance.New(class.DomainError, map[string]ilos.Instance{
			"OBJECT":         lambdaList,
			"EXPECTED-CLASS": class.List,
		})
	}
	lexical := local
	cdr := lambdaList
	parameters := []ilos.Instance{}
	variadic := false
	for instance.Of(class.Cons, cdr) {
		cadr := instance.UnsafeCar(cdr)
		if cadr == instance.New(class.Symbol, ":REST") || cadr == instance.New(class.Symbol, "&REST") {
			variadic = true
		}
		parameters = append(parameters, cadr)
		cdr = instance.UnsafeCdr(cdr)
	}
	name := instance.New(class.Symbol, "ANONYMOUS-FUNCTION")
	return instance.New(class.Function, name, func(local, global *environment.Environment, arguments ...ilos.Instance) (ilos.Instance, ilos.Instance) {
		local.BlockTag = append(lexical.BlockTag, local.BlockTag...)
		local.TagbodyTag = append(lexical.TagbodyTag, local.TagbodyTag...)
		local.CatchTag = append(lexical.CatchTag, local.CatchTag...)
		local.Variable = append(lexical.Variable, local.Variable...)
		local.Function = append(lexical.Function, local.Function...)
		local.Special = append(lexical.Special, local.Special...)
		local.Macro = append(lexical.Macro, local.Macro...)
		local.DynamicVariable = append(lexical.DynamicVariable, local.DynamicVariable...)
		if (variadic && len(parameters)-2 > len(arguments)) || (!variadic && len(parameters) != len(arguments)) {
			v := instance.New(class.Null)
			for i := len(arguments) - 1; i >= 0; i-- {
				v = instance.New(class.Cons, arguments[i], v)
			}
			return nil, instance.New(class.WrongNumberOfArguments, map[string]ilos.Instance{
				"FORM":      name,
				"ARGUMENTS": v,
			})
		}
		for idx := range parameters {
			key := parameters[idx]
			if key == instance.New(class.Symbol, ":REST") || key == instance.New(class.Symbol, "&REST") {
				key := parameters[idx+1]
				value := instance.New(class.Null)
				for i := len(arguments) - 1; i >= idx; i-- {
					value = instance.New(class.Cons, arguments[i], value)
				}
				local.Variable.Define(key, value)
				break
			}
			value := arguments[idx]
			local.Variable.Define(key, value)
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
