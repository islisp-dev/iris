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

type Validator = func (core.Instance) error

func Any(_ core.Instance) error {
	return nil
}

func InstanceOf(class core.Class) Validator {
	return func (instance core.Instance) error {
		ok := core.InstanceOf(class, instance)
		if !ok {
			return ValidationError{instance, class}
		}
		return nil
	}
}

func Symbol(pattern string) Validator {
	re := regexp2.MustCompile(pattern, 0)
	return func (instance core.Instance) error {
		symbol, ok := instance.(core.Symbol)
		if !ok {
			return ValidationError{instance, core.SymbolClass} 
		}
		ok, err := re.MatchString(symbol.String())
		if err != nil || !ok {
			return ValidationError{instance, core.SymbolClass}
		}
		return nil
	}
}

func Or(validators ...Validator) Validator {
	return func (instance core.Instance) error {
		for _, validator := range validators {
			if err := validator(instance); err == nil {
				return nil
			}
		}
		return ValidationError{instance, core.ObjectClass}
	}
}

func And(validators ...Validator) Validator {
	return func (instance core.Instance) error {
		for _, validator := range validators {
			if err := validator(instance); err != nil {
				return err
			}
		}
		return nil
	}
}

func Not(validator Validator) Validator {
	return func (instance core.Instance) error {
		if err := validator(instance); err != nil {
			return nil
		}
		return ValidationError{instance, core.ObjectClass}
	}
}

func List(validators ...Validator) Validator {
	return func (args core.Instance) error {
		if len(validators) == 0 && args == core.Nil {
			return nil
		}
		if len(validators) == 0 || args == core.Nil {
			return ValidationError{args, core.ListClass}
		}
		cons, ok := args.(*core.Cons)
		if !ok {
			return ValidationError{args, core.ConsClass}
		}
		if err := validators[0](cons.Car); err != nil {
			return err
		}
		if err := List(validators...)(cons.Cdr); err != nil {
			return err
		}
		return nil
	}
}

var Nil = List()

func Append(validators ...Validator) Validator {
	return func (args core.Instance) error {
		if len(validators) == 0 {
			return nil
		}
		rest := args
		for {
			cons, ok := rest.(*core.Cons)
			if !ok {
				return ValidationError{rest, core.ConsClass}
			}
			rest := cons.Cdr
			cons.Cdr = core.Nil
			if err := validators[0](args); err != nil {
				cons.Cdr = rest
				continue
			}
			if err := Append(validators...)(rest); err != nil {
				cons.Cdr = rest
				continue
			}
			return nil
		}
	}
}

func Repeat(inner Validator) (outer Validator) {
	outer = func (args core.Instance) error {
		if args == core.Nil {
			return nil
		}
		cons, ok := args.(*core.Cons)
		if !ok {
			return ValidationError{args, core.ConsClass} 
		}
		if err := inner(cons.Car); err != nil {
			return err
		}
		if err := outer(cons.Cdr); err != nil {
			return err
		}
		return nil
	}
	return
}
