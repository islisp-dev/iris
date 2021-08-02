package validator

import (
	"github.com/dlclark/regexp2"
	"github.com/islisp-dev/iris/core"
)

type ValidationError struct {
	Actual core.Instance
	Expected core.Instance
}

func (err ValidationError) Error() string {
	return "Validation Error"
}

type Validator = func (core.Instance) (bool, error)

func Any(_ core.Instance) (bool, error) {
	return true, nil
}

func InstanceOf(class core.Class) Validator {
	return func (instance core.Instance) (bool, error) {
		ok := core.InstanceOf(class, instance)
		if !ok {
			return ok, ValidationError{instance, class}
		}
		return ok, nil
	}
}

func Symbol(pattern string) Validator {
	re := regexp2.MustCompile(pattern, 0)
	return func (instance core.Instance) (bool, error) {
		symbol, ok := instance.(core.Symbol)
		if !ok {
			return false, ValidationError{instance, core.SymbolClass} 
		}
		ok, err := re.MatchString(symbol.String())
		if err != nil || !ok {
			return false, ValidationError{instance, core.SymbolClass}
		}
		return true, nil
	}
}

func Or(validators ...Validator) Validator {
	return func (instance core.Instance) (bool, error) {
		for _, validator := range validators {
			ok, err := validator(instance)
			if ok {
				return ok, err
			}
		}
		return false, ValidationError{instance, core.ObjectClass}
	}
}

func And(validators ...Validator) Validator {
	return func (instance core.Instance) (bool, error) {
		for _, validator := range validators {
			ok, err := validator(instance)
			if !ok {
				return ok, err
			}
		}
		return true, nil
	}
}

func List(validators ...Validator) Validator {
	return func (args core.Instance) (bool, error) {
		if len(validators) == 0 && args == core.Nil {
			return true, nil
		}
		if len(validators) == 0 || args == core.Nil {
			return false, ValidationError{args, core.ListClass}
		}
		cons, ok := args.(*core.Cons)
		if !ok {
			return false, ValidationError{args, core.ConsClass}
		}
		ok, err := validators[0](cons.Car)
		if err != nil || !ok {
			return false, err
		}
		ok, err = List(validators...)(cons.Cdr)
		if err != nil || !ok {
			return false, err
		}
		return true, nil
	}
}

func Append(validators ...Validator) Validator {
	return func (args core.Instance) (bool, error) {
		if len(validators) == 0 {
			return false, nil
		}
		rest := args
		for {
			cons, ok := rest.(*core.Cons)
			if !ok {
				return false, ValidationError{rest, core.ConsClass}
			}
			rest := cons.Cdr
			cons.Cdr = core.Nil
			ok, err := validators[0](args)
			if !ok || err != nil {
				cons.Cdr = rest
				continue
			}
			ok, err = Append(validators...)(rest)
			if !ok || err != nil {
				cons.Cdr = rest
				continue
			}
			return true, nil
		}
	}
}

func Repeat(inner Validator) (outer Validator) {
	outer = func (args core.Instance) (bool, error) {
		if args == core.Nil {
			return true, nil
		}
		cons, ok := args.(*core.Cons)
		if !ok {
			return false, ValidationError{args, core.ConsClass} 
		}
		ok, err := inner(cons.Car)
		if err != nil || !ok {
			return false, err
		}
		ok, err = outer(cons.Cdr)
		if err != nil || !ok {
			return false, err
		}
		return true, nil
	}
	return
}
