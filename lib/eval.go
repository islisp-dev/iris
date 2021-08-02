// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.

package lib

import "github.com/islisp-dev/iris/core"

func evalArguments(e core.Environment, arguments core.Instance) (core.Instance, core.Instance) {
	// if arguments ends here
	if core.DeepEqual(arguments, Nil) {
		return Nil, nil
	}
	if err := ensure(e, core.ConsClass, arguments); err != nil {
		return nil, err
	}
	car := arguments.(*core.Cons).Car // Checked there
	cdr := arguments.(*core.Cons).Cdr // Checked there
	a, err := Eval(e, car)
	if err != nil {
		return nil, err
	}
	b, err := evalArguments(e, cdr)
	if err != nil {
		return nil, err
	}
	return core.NewCons(a, b), nil

}

func evalLambda(e core.Environment, car, cdr core.Instance) (core.Instance, core.Instance, bool) {
	// eval if lambda form
	if core.InstanceOf(core.ConsClass, car) {
		caar := car.(*core.Cons).Car // Checked at the top of// This sentence
		if core.DeepEqual(caar, core.NewSymbol("LAMBDA")) {
			fun, err := Eval(e, car)
			if err != nil {
				st, ok := err.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), core.SeriousConditionClass)
				if !ok {
					st = Nil
				}
				l, c := caar.(core.Symbol).Location()
				loc := core.NewCons(core.NewCons(core.NewInteger(l), core.NewInteger(c)), st)
				err.(core.BasicInstance).SetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), loc, core.SeriousConditionClass)
				return nil, err, true
			}

			arguments, err := evalArguments(e, cdr)
			if err != nil {
				st, ok := err.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), core.SeriousConditionClass)
				if !ok {
					st = Nil
				}
				l, c := caar.(core.Symbol).Location()
				loc := core.NewCons(core.NewCons(core.NewInteger(l), core.NewInteger(c)), st)
				err.(core.BasicInstance).SetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), loc, core.SeriousConditionClass)
				return nil, err, true
			}
			ret, err := fun.(core.Applicable).Apply(e.NewDynamic(), arguments.(core.List).Slice()...)
			if err != nil {
				st, ok := err.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), core.SeriousConditionClass)
				if !ok {
					st = Nil
				}
				l, c := caar.(core.Symbol).Location()
				loc := core.NewCons(core.NewCons(core.NewInteger(l), core.NewInteger(c)), st)
				err.(core.BasicInstance).SetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), loc, core.SeriousConditionClass)
				return nil, err, true
			}
			return ret, nil, true
		}
	}
	return nil, nil, false
}

func evalSpecial(e core.Environment, car, cdr core.Instance) (core.Instance, core.Instance, bool) {
	// get special instance has value of Function interface
	var spl core.Instance
	if s, ok := e.Special.Get(car); ok {
		spl = s
	}
	if spl != nil {
		ret, err := spl.(core.Applicable).Apply(e.NewLexical(), cdr.(core.List).Slice()...)
		if err != nil {
			st, ok := err.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), core.SeriousConditionClass)
			if !ok {
				st = Nil
			}
			l, c := car.(core.Symbol).Location()
			loc := core.NewCons(core.NewCons(core.NewInteger(l), core.NewInteger(c)), st)
			err.(core.BasicInstance).SetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), loc, core.SeriousConditionClass)
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalMacro(e core.Environment, car, cdr core.Instance) (core.Instance, core.Instance, bool) {
	// get special instance has value of Function interface
	var mac core.Instance
	if m, ok := e.Macro.Get(car); ok {
		mac = m
	}
	if mac != nil {
		ret, err := mac.(core.Applicable).Apply(e.NewDynamic(), cdr.(core.List).Slice()...)
		if err != nil {
			st, ok := err.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), core.SeriousConditionClass)
			if !ok {
				st = Nil
			}
			l, c := car.(core.Symbol).Location()
			loc := core.NewCons(core.NewCons(core.NewInteger(l), core.NewInteger(c)), st)
			err.(core.BasicInstance).SetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), loc, core.SeriousConditionClass)
			return nil, err, true
		}
		ret, err = Eval(e, ret)
		if err != nil {
			st, ok := err.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), core.SeriousConditionClass)
			if !ok {
				st = Nil
			}
			l, c := car.(core.Symbol).Location()
			loc := core.NewCons(core.NewCons(core.NewInteger(l), core.NewInteger(c)), st)
			err.(core.BasicInstance).SetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), loc, core.SeriousConditionClass)
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalFunction(e core.Environment, car, cdr core.Instance) (core.Instance, core.Instance, bool) {
	// get special instance has value of Function interface
	var fun core.Instance
	if f, ok := e.Function.Get(car); ok {
		fun = f
	}
	if fun != nil {
		arguments, err := evalArguments(e, cdr)
		if err != nil {
			st, ok := err.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), core.SeriousConditionClass)
			if !ok {
				st = Nil
			}
			l, c := car.(core.Symbol).Location()
			loc := core.NewCons(core.NewCons(core.NewInteger(l), core.NewInteger(c)), st)
			err.(core.BasicInstance).SetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), loc, core.SeriousConditionClass)
			return nil, err, true
		}
		ret, err := fun.(core.Applicable).Apply(e.NewDynamic(), arguments.(core.List).Slice()...)
		if err != nil {
			st, ok := err.(core.BasicInstance).GetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), core.SeriousConditionClass)
			if !ok {
				st = Nil
			}
			l, c := car.(core.Symbol).Location()
			loc := core.NewCons(core.NewCons(core.NewInteger(l), core.NewInteger(c)), st)
			err.(core.BasicInstance).SetSlotValue(core.NewSymbol("IRIS.STACKTRACE"), loc, core.SeriousConditionClass)
			return nil, err, true
		}
		return ret, nil, true
	}
	return nil, nil, false
}

func evalCons(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if err := ensure(e, core.ConsClass, obj); err != nil {
		return nil, err
	}
	if !isProperList(obj) {
		return SignalCondition(e, core.NewParseError(e, obj, core.ListClass), Nil)
	}
	car := obj.(*core.Cons).Car // Checked at the top of// This function
	cdr := obj.(*core.Cons).Cdr // Checked at the top of// This function

	// eval if lambda form
	if a, b, c := evalLambda(e, car, cdr); c {
		return a, b
	}
	// get special instance has value of Function interface
	if a, b, c := evalSpecial(e, car, cdr); c {
		return a, b
	}
	// get macro instance has value of Function interface
	if a, b, c := evalMacro(e, car, cdr); c {
		return a, b
	}
	// get function instance has value of Function interface
	if a, b, c := evalFunction(e, car, cdr); c {
		return a, b
	}
	err := core.NewUndefinedFunction(e, car)
	return SignalCondition(e, err, Nil)
}

func evalVariable(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if val, ok := e.Variable.Get(obj); ok {
		return val, nil
	}
	if val, ok := e.Constant.Get(obj); ok {
		return val, nil
	}
	err := core.NewUnboundVariable(e, obj)
	return SignalCondition(e, err, Nil)
}

// Eval evaluates any classs
func Eval(e core.Environment, obj core.Instance) (core.Instance, core.Instance) {
	if core.DeepEqual(obj, Nil) {
		return Nil, nil
	}
	if core.InstanceOf(core.SymbolClass, obj) {
		ret, err := evalVariable(e, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	if core.InstanceOf(core.ConsClass, obj) {
		ret, err := evalCons(e, obj)
		if err != nil {
			return nil, err
		}
		return ret, nil
	}
	return obj, nil
}
